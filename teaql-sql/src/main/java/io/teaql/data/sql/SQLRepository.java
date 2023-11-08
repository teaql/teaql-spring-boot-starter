package io.teaql.data.sql;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.text.NamingCase;
import cn.hutool.core.util.*;
import io.teaql.data.*;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.PropertyType;
import io.teaql.data.meta.Relation;
import io.teaql.data.repository.AbstractRepository;
import io.teaql.data.sql.expression.ExpressionHelper;
import io.teaql.data.sql.expression.SQLExpressionParser;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import javax.sql.DataSource;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.TransactionTemplate;

public class SQLRepository<T extends Entity> extends AbstractRepository<T>
    implements SQLColumnResolver {
  private String childType = "_child_type";
  private String childSqlType = "VARCHAR(100)";
  private String tqlIdSpaceTable = "teaql_id_space";

  public static final String MULTI_TABLE = "MULTI_TABLE";

  private final EntityDescriptor entityDescriptor;

  public DataSource getDataSource() {
    return dataSource;
  }

  private final DataSource dataSource;
  private final NamedParameterJdbcTemplate jdbcTemplate;
  private String versionTableName;
  private List<String> primaryTableNames = new ArrayList<>();
  private String thisPrimaryTableName;
  private Set<String> allTableNames = new LinkedHashSet<>();
  private List<String> types = new ArrayList<>();
  private List<String> auxiliaryTableNames;
  private List<PropertyDescriptor> allProperties = new ArrayList<>();

  private Map<Class, SQLExpressionParser> expressionParsers = new ConcurrentHashMap<>();

  public SQLRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    this.entityDescriptor = entityDescriptor;
    this.dataSource = dataSource;
    this.jdbcTemplate = new NamedParameterJdbcTemplate(dataSource);
    initSQLMeta(entityDescriptor);
    initExpressionParsers(entityDescriptor, dataSource);
  }

  protected void initExpressionParsers(EntityDescriptor entityDescriptor, DataSource dataSource) {
    Set<Class<?>> parsers =
        ClassUtil.scanPackageBySuper(
            ExpressionHelper.class.getPackageName(), SQLExpressionParser.class);
    for (Class<?> parser : parsers) {
      if (!ClassUtil.isInterface(parser) && !ClassUtil.isAbstract(parser)) {
        SQLExpressionParser o = (SQLExpressionParser) ReflectUtil.newInstance(parser);
        registerExpressionParser(o);
      }
    }
  }

  public void registerExpressionParser(SQLExpressionParser sqlExpressionParser) {
    if (sqlExpressionParser == null) {
      return;
    }
    Class type = sqlExpressionParser.type();
    if (type != null) {
      expressionParsers.put(type, sqlExpressionParser);
    }
  }

  public void registerExpressionParser(Class<? extends SQLExpressionParser> parser) {
    if (!ClassUtil.isInterface(parser) && !ClassUtil.isAbstract(parser)) {
      SQLExpressionParser o = ReflectUtil.newInstance(parser);
      registerExpressionParser(o);
    }
  }

  private void initSQLMeta(EntityDescriptor entityDescriptor) {
    EntityDescriptor descriptor = entityDescriptor;
    while (descriptor != null) {
      types.add(descriptor.getType());
      List<PropertyDescriptor> properties = descriptor.getProperties();
      for (PropertyDescriptor property : properties) {
        allProperties.add(property);
        if (property instanceof Relation && !shouldHandle((Relation) property)) {
          continue;
        }
        List<SQLColumn> sqlColumns = getSqlColumns(property);
        if (ObjectUtil.isEmpty(sqlColumns)) {
          throw new RepositoryException(
              "property :" + property.getName() + " miss sql table columns");
        }

        String firstTable = sqlColumns.get(0).getTableName();
        if (property.isVersion()) {
          this.versionTableName = firstTable;
        }
        if (property.isId()) {
          if (!this.primaryTableNames.contains(firstTable)) {
            this.primaryTableNames.add(firstTable);
          }
          if (property.getOwner() == this.entityDescriptor) {
            this.thisPrimaryTableName = firstTable;
          }
        }
        this.allTableNames.addAll(CollStreamUtil.toList(sqlColumns, SQLColumn::getTableName));
      }
      descriptor = descriptor.getParent();
    }
    this.auxiliaryTableNames =
        new ArrayList<>(CollectionUtil.subtract(this.allTableNames, this.primaryTableNames));
  }

  @Override
  public EntityDescriptor getEntityDescriptor() {
    return this.entityDescriptor;
  }

  public void updateInternal(UserContext userContext, Collection<T> updateItems) {
    if (ObjectUtil.isEmpty(updateItems)) {
      return;
    }
    List<SQLEntity> sqlEntities =
        CollectionUtil.map(updateItems, i -> convertToSQLEntityForUpdate(userContext, i), true);
    if (ObjectUtil.isEmpty(sqlEntities)) {
      return;
    }
    for (SQLEntity sqlEntity : sqlEntities) {
      if (sqlEntity.isEmpty()) {
        continue;
      }
      Map<String, List<String>> tableColumnNames = sqlEntity.getTableColumnNames();
      Map<String, List> tableColumnValues = sqlEntity.getTableColumnValues();

      // versionTableUpdated flag
      AtomicBoolean versionTableUpdated = new AtomicBoolean(false);
      tableColumnValues.forEach(
          (k, v) -> {
            List<String> columns = new ArrayList<>(tableColumnNames.get(k));

            List l = new ArrayList(v);

            boolean versionTable = this.versionTableName.equals(k);
            boolean primaryTable = this.primaryTableNames.contains(k);
            if (versionTable) {
              updateVersionTable(userContext, sqlEntity, versionTableUpdated, k, columns, l);
            } else if (primaryTable) {
              updatePrimaryTable(userContext, sqlEntity, k, columns, l);
            } else {
              try {
                jdbcTemplate
                    .getJdbcTemplate()
                    .update(prepareSubsidiaryTableSql(k, columns), l.toArray(new Object[0]));
              } catch (DataAccessException pE) {
                throw new RepositoryException(pE);
              }
            }
          });

      // 如果没有更新version table属性，此处我们只更新version table的版本
      if (!versionTableUpdated.get()) {
        updateVersionTableVersion(userContext, sqlEntity);
      }
    }
  }

  public String prepareSubsidiaryTableSql(String tableName, List<String> tableColumns) {
    return StrUtil.format(
        "REPLACE INTO {} SET {}",
        tableName,
        tableColumns.stream().map(c -> c + " = ?").collect(Collectors.joining(" , ")));
  }

  private void updateVersionTableVersion(UserContext userContext, SQLEntity sqlEntity) {
    String updateSql =
        StrUtil.format(
            "UPDATE {} SET {} = ？ WHERE {} = ? and {} = ?",
            this.versionTableName,
            VERSION,
            ID,
            VERSION);
    Object[] parameters = {sqlEntity.getVersion() + 1, sqlEntity.getId(), sqlEntity.getVersion()};
    int update;
    try {
      update = jdbcTemplate.getJdbcTemplate().update(updateSql, parameters);
    } catch (DataAccessException pE) {
      throw new RepositoryException(pE);
    }
    SQLLogger.logSQLAndParameters(userContext, updateSql, parameters, update + " UPDATED");
    if (update != 1) {
      throw new ConcurrentModifyException();
    }
  }

  private void updatePrimaryTable(
      UserContext userContext, SQLEntity sqlEntity, String k, List<String> columns, List l) {
    l.add(sqlEntity.getId());
    String updateSql =
        StrUtil.format(
            "UPDATE {} SET {} WHERE id = ?",
            k,
            columns.stream().map(c -> c + " = ?").collect(Collectors.joining(" , ")));
    Object[] parameters = l.toArray(new Object[0]);
    int update;
    try {
      update = jdbcTemplate.getJdbcTemplate().update(updateSql, parameters);
    } catch (DataAccessException pE) {
      throw new RepositoryException(pE);
    }
    SQLLogger.logSQLAndParameters(userContext, updateSql, parameters, update + " UPDATED");
    if (update != 1) {
      throw new RepositoryException("primary table update failed");
    }
  }

  private void updateVersionTable(
      UserContext userContext,
      SQLEntity sqlEntity,
      AtomicBoolean versionTableUpdated,
      String k,
      List<String> columns,
      List l) {
    // version表已更新
    versionTableUpdated.set(true);
    // 增加version列的修改
    columns.add(VERSION);
    l.add(sqlEntity.getVersion() + 1); // 版本加1
    l.add(sqlEntity.getId());
    l.add(sqlEntity.getVersion());
    String updateSql =
        StrUtil.format(
            "UPDATE {} SET {} WHERE id = ? AND version = ?",
            k,
            columns.stream().map(c -> c + " = ?").collect(Collectors.joining(" , ")));
    Object[] parameters = l.toArray(new Object[0]);
    int update;
    try {
      update = jdbcTemplate.getJdbcTemplate().update(updateSql, parameters);
    } catch (DataAccessException pE) {
      throw new RepositoryException(pE);
    }
    SQLLogger.logSQLAndParameters(userContext, updateSql, parameters, update + " UPDATED");
    if (update != 1) {
      throw new ConcurrentModifyException();
    }
  }

  private SQLEntity convertToSQLEntityForUpdate(UserContext userContext, T entity) {
    // 只更新有变化的属性
    List<String> updatedProperties = entity.getUpdatedProperties();
    if (ObjectUtil.isEmpty(updatedProperties)) {
      return null;
    }
    SQLEntity sqlEntity = new SQLEntity();
    sqlEntity.setId(entity.getId());
    sqlEntity.setVersion(entity.getVersion());
    for (String updatedProperty : updatedProperties) {
      PropertyDescriptor property = findProperty(updatedProperty);
      // id只能在新建时设置， version由系统维护，不能更改
      if (property.isId() || property.isVersion()) {
        continue;
      }
      Object v = entity.getProperty(property.getName());
      List<SQLData> data = convertToSQLData(userContext, property, v);
      sqlEntity.addPropertySQLData(data);
    }
    return sqlEntity;
  }

  public void createInternal(UserContext userContext, Collection<T> createItems) {
    List<SQLEntity> sqlEntities =
        CollectionUtil.map(createItems, i -> convertToSQLEntityForInsert(userContext, i), true);
    if (ObjectUtil.isEmpty(sqlEntities)) {
      return;
    }

    SQLEntity sqlEntity = sqlEntities.get(0);

    // collect table/columns for the first entity(all entities with the same structure)
    Map<String, List<String>> tableColumns = sqlEntity.getTableColumnNames();

    // collect all rows for entities, we will insert them in the batch
    Map<String, List<Object[]>> rows = new HashMap<>();
    for (SQLEntity entity : sqlEntities) {
      Map<String, List> tableColumnValues = entity.getTableColumnValues();
      tableColumnValues.forEach(
          (k, v) -> {
            List<Object[]> values = rows.get(k);
            if (values == null) {
              values = new ArrayList<>();
              rows.put(k, values);
            }
            // for auxiliary tables, we only save the row if there is values except id
            if (auxiliaryTableNames.contains(k) && entity.allNullExceptID(v)) {
              return;
            }
            values.add(v.toArray(new Object[0]));
          });
    }

    rows.forEach(
        (k, v) -> {
          if (v.isEmpty()) {
            return;
          }
          List<String> columns = tableColumns.get(k);
          String sql =
              StrUtil.format(
                  "INSERT INTO {} ({}) VALUES ({})",
                  k,
                  CollectionUtil.join(columns, ","),
                  StrUtil.repeatAndJoin("?", columns.size(), ","));
          int[] rets;
          try {
            rets = jdbcTemplate.getJdbcTemplate().batchUpdate(sql, v);
          } catch (DataAccessException pE) {
            throw new RepositoryException(pE);
          }
          int i = 0;
          for (int ret : rets) {
            SQLLogger.logSQLAndParameters(userContext, sql, v.get(i++), ret + " UPDATED");
          }
        });
  }

  private SQLEntity convertToSQLEntityForInsert(UserContext userContext, T entity) {
    SQLEntity sqlEntity = new SQLEntity();
    sqlEntity.setId(entity.getId());
    sqlEntity.setVersion(entity.getVersion());

    for (PropertyDescriptor propertyDescriptor : this.allProperties) {
      if (propertyDescriptor instanceof Relation) {
        if (!shouldHandle((Relation) propertyDescriptor)) {
          continue;
        }
      }
      Object v = entity.getProperty(propertyDescriptor.getName());
      List<SQLData> data = convertToSQLData(userContext, propertyDescriptor, v);
      sqlEntity.addPropertySQLData(data);
    }

    for (int i = 0; i < this.types.size() - 1; i++) {
      String tableName = this.primaryTableNames.get(i + 1);
      String type = this.types.get(i);
      SQLData childTypeCell = new SQLData();
      childTypeCell.setTableName(tableName);
      childTypeCell.setColumnName(getChildType());
      childTypeCell.setValue(type);
      sqlEntity.addPropertySQLData(childTypeCell);
    }
    return sqlEntity;
  }

  private List<SQLData> convertToSQLData(
      UserContext pUserContext, PropertyDescriptor property, Object propertyValue) {
    if (property instanceof SQLProperty) {
      return ((SQLProperty) property).toDBRaw(propertyValue);
    }
    throw new RepositoryException("SQLRepository 目前只支持SQLProperty");
  }

  private Object toSQLValue(Entity entity, PropertyDescriptor property) {
    return entity.getProperty(property.getName());
  }

  private Object toDBValue(Object property, PropertyDescriptor pProperty) {
    return pProperty;
  }

  @Override
  public void deleteInternal(UserContext userContext, Collection<T> entities) {
    if (ObjectUtil.isNotEmpty(entities)) {
      return;
    }
    List<Object[]> args =
        entities.stream()
            .filter(e -> e.getVersion() > 0)
            .map(
                e ->
                    new Object[] {
                      // 版本+1 然后取反。即version的绝对值表示修改次数，版本为负表示已被删除
                      -(e.getVersion() + 1), e.getId(), e.getVersion()
                    })
            .collect(Collectors.toList());
    String updateSql =
        StrUtil.format(
            "UPDATE {} SET version = ? WHERE id = ? AND version = ?", this.versionTableName);
    int[] rets;
    try {
      rets = jdbcTemplate.getJdbcTemplate().batchUpdate(updateSql, args);
    } catch (DataAccessException pE) {
      throw new RepositoryException(pE);
    }
    int i = 0;
    for (int ret : rets) {
      SQLLogger.logSQLAndParameters(userContext, updateSql, args.get(i++), ret + " UPDATED");
      if (ret != 1) {
        throw new ConcurrentModifyException();
      }
    }
  }

  @Override
  public void recoverInternal(UserContext userContext, Collection<T> entities) {
    if (ObjectUtil.isNotEmpty(entities)) {
      return;
    }
    List<Object[]> args =
        entities.stream()
            .filter(e -> e.getVersion() < 0)
            .map(
                e ->
                    new Object[] {
                      // 版本取反加1。即version的绝对值表示修改次数，版本为负表示已被删除
                      (-e.getVersion() + 1), e.getId(), e.getVersion()
                    })
            .collect(Collectors.toList());
    String updateSql =
        StrUtil.format(
            "UPDATE {} SET version = ? WHERE id = ? AND version = ?", this.versionTableName);
    int[] rets;
    try {
      rets = jdbcTemplate.getJdbcTemplate().batchUpdate(updateSql, args);
    } catch (DataAccessException pE) {
      throw new RepositoryException(pE);
    }
    int i = 0;
    for (int ret : rets) {
      SQLLogger.logSQLAndParameters(userContext, updateSql, args.get(i++), ret + " UPDATED");
      if (ret != 1) {
        throw new ConcurrentModifyException();
      }
    }
  }

  private SQLColumn getSqlColumn(PropertyDescriptor property) {
    List<SQLColumn> sqlColumns = getSqlColumns(property);
    SQLColumn sqlColumn = CollectionUtil.getFirst(sqlColumns);
    return sqlColumn;
  }

  private List<SQLColumn> getSqlColumns(PropertyDescriptor property) {
    if (property instanceof SQLProperty) {
      return ((SQLProperty) property).columns();
    }
    throw new RepositoryException("SQLRepository 目前只支持SQLProperty");
  }

  public SmartList<T> loadInternal(UserContext userContext, SearchRequest<T> request) {
    Map<String, Object> params = new HashMap<>();
    String sql = buildDataSQL(userContext, request, params);
    List<T> results = new ArrayList<>();
    if (!ObjectUtil.isEmpty(sql)) {
      try {
        results = jdbcTemplate.query(sql, params, getMapper(userContext, request));
      } catch (DataAccessException pE) {
        throw new RepositoryException(pE);
      }
      SQLLogger.logNamedSQL(userContext, sql, params, results);
    }
    SmartList<T> smartList = new SmartList<>(results);
    return smartList;
  }

  protected AggregationResult aggregateInternal(UserContext userContext, SearchRequest<T> request) {
    if (!request.hasSimpleAgg()) {
      return null;
    }
    List<String> tables = collectAggregationTables(userContext, request);
    Map<String, Object> parameters = new HashMap();

    if (ObjectUtil.isEmpty(tables)) {
      tables = new ArrayList<>(ListUtil.of(thisPrimaryTableName));
    }
    String idTable = tables.get(0);
    userContext.put(MULTI_TABLE, tables.size() > 1);

    String whereSql =
        prepareCondition(userContext, idTable, request.getSearchCriteria(), parameters);

    if (SearchCriteria.FALSE.equalsIgnoreCase(whereSql)) {
      return null;
    }

    String selectSql = collectAggregationSelectSql(userContext, request, idTable, parameters);
    String sql =
        StrUtil.format("SELECT {} FROM {}", selectSql, leftJoinTables(userContext, tables));

    if (whereSql != null && !SearchCriteria.TRUE.equalsIgnoreCase(whereSql)) {
      sql = StrUtil.format("{} WHERE {}", sql, whereSql);
    }

    String groupBy = collectAggregationGroupBySql(userContext, request, idTable, parameters);
    if (!ObjectUtil.isEmpty(groupBy)) {
      sql = StrUtil.format("{} {}", sql, groupBy);
    }

    userContext.info(sql);
    userContext.info("{}", parameters);
    List<AggregationItem> aggregationItems;
    try {
      aggregationItems = jdbcTemplate.query(sql, parameters, getAggregationMapper(request));
    } catch (DataAccessException pE) {
      throw new RepositoryException(pE);
    }
    AggregationResult result = new AggregationResult();
    result.setName(request.getAggregations().getName());
    result.setData(aggregationItems);
    return result;
  }

  private RowMapper<AggregationItem> getAggregationMapper(SearchRequest<T> request) {
    return (rs, index) -> {
      AggregationItem item = new AggregationItem();
      Aggregations aggregations = request.getAggregations();
      List<SimpleNamedExpression> functions = aggregations.getAggregates();
      List<SimpleNamedExpression> dimensions = aggregations.getDimensions();
      for (SimpleNamedExpression function : functions) {
        item.addValue(function, rs.getObject(function.name()));
      }
      for (SimpleNamedExpression dimension : dimensions) {
        item.addDimension(dimension, rs.getObject(dimension.name()));
      }
      return item;
    };
  }

  private String collectAggregationGroupBySql(
      UserContext userContext,
      SearchRequest<T> request,
      String idTable,
      Map<String, Object> parameters) {
    List<SimpleNamedExpression> dimensions = request.getAggregations().getDimensions();
    if (dimensions.isEmpty()) {
      return null;
    }
    return dimensions.stream()
        .map(
            dimension -> {
              Expression expression = dimension.getExpression();
              while (expression instanceof SimpleNamedExpression) {
                expression = dimension.getExpression();
              }
              return expression;
            })
        .map(
            expression ->
                ExpressionHelper.toSql(userContext, expression, idTable, parameters, this))
        .collect(Collectors.joining(",", "GROUP BY ", ""));
  }

  private String collectAggregationSelectSql(
      UserContext userContext,
      SearchRequest<T> request,
      String idTable,
      Map<String, Object> params) {
    List<SimpleNamedExpression> allSelected = request.getAggregations().getSelectedExpressions();
    return allSelected.stream()
        .map(expression -> ExpressionHelper.toSql(userContext, expression, idTable, params, this))
        .collect(Collectors.joining(","));
  }

  private List<String> collectAggregationTables(UserContext userContext, SearchRequest<T> request) {
    return collectTablesFromProperties(userContext, request.aggregationProperties(userContext));
  }

  private RowMapper<T> getMapper(UserContext pUserContext, SearchRequest<T> pRequest) {
    return (rs, rowIndex) -> {
      Class<? extends T> returnType = pRequest.returnType();
      T entity = ReflectUtil.newInstance(returnType);
      for (PropertyDescriptor property : this.allProperties) {
        setProperty(pUserContext, entity, property, rs);
      }

      List<SimpleNamedExpression> simpleDynamicProperties = pRequest.getSimpleDynamicProperties();
      for (SimpleNamedExpression simpleDynamicProperty : simpleDynamicProperties) {
        String name = simpleDynamicProperty.name();
        entity.addDynamicProperty(name, rs.getObject(name));
      }

      if (entity.getVersion() < 0) {
        if (entity instanceof BaseEntity) {
          ((BaseEntity) entity).set$status(EntityStatus.PERSISTED_DELETED);
        }
      } else {
        if (entity instanceof BaseEntity) {
          ((BaseEntity) entity).set$status(EntityStatus.PERSISTED);
        }
      }
      return entity;
    };
  }

  private void setProperty(
      UserContext userContext, T pEntity, PropertyDescriptor pProperty, ResultSet resultSet) {
    if (!shouldHandle(pProperty)) {
      return;
    }

    if (pProperty instanceof SQLProperty) {
      ((SQLProperty) pProperty).setPropertyValue(pEntity, resultSet);
      return;
    }
    throw new RepositoryException(
        "SQLRepository属性[" + pProperty.getName() + "]错误，目前只支持SQLProperty");
  }

  private boolean shouldHandle(PropertyDescriptor pProperty) {
    if (pProperty instanceof Relation) {
      return shouldHandle((Relation) pProperty);
    }
    return true;
  }

  public String buildDataSQL(
      UserContext userContext, SearchRequest request, Map<String, Object> parameters) {

    // collect tables from the request
    List<String> tables = collectDataTables(userContext, request);

    if (ObjectUtil.isEmpty(tables)) {
      tables = new ArrayList<>(ListUtil.of(thisPrimaryTableName));
    }

    // pick the first the table as the id table(all tables have id column)
    String idTable = tables.get(0);
    userContext.put(MULTI_TABLE, tables.size() > 1);

    // condition
    String whereSql =
        prepareCondition(userContext, idTable, request.getSearchCriteria(), parameters);

    // condition is false, no result
    if (SearchCriteria.FALSE.equalsIgnoreCase(whereSql)) {
      return null;
    }

    // no data is required
    if (request.getSlice() != null && request.getSlice().getSize() == 0) {
      return null;
    }

    String tableSQl = leftJoinTables(userContext, tables);

    // selects
    String selectSql = collectSelectSql(userContext, request, idTable, parameters);

    String partitionProperty = request.getPartitionProperty();
    if (ObjectUtil.isNotEmpty(partitionProperty) && request.getSlice() != null) {
      ensureOrderByForPartition(request);
    }

    // 排序
    String orderBySql = prepareOrderBy(userContext, request, idTable, parameters);
    if (!ObjectUtil.isEmpty(partitionProperty) && request.getSlice() != null) {
      PropertyDescriptor partitionPropertyDescriptor = findProperty(partitionProperty);
      SQLColumn sqlColumn = getSqlColumn(partitionPropertyDescriptor);
      String partitionTable;
      if (partitionPropertyDescriptor.isId()) {
        partitionTable = idTable;
      } else {
        partitionTable = sqlColumn.getTableName();
      }

      if (whereSql != null) {
        whereSql = "WHERE " + whereSql;
      }

      return StrUtil.format(
          "SELECT * FROM (SELECT {}, (row_number() over(partition by {}.{} {})) as _rank from {} {}) as t where t._rank >= {} and t._rank < {}",
          selectSql,
          tableAlias(partitionTable),
          sqlColumn.getColumnName(),
          orderBySql,
          tableSQl,
          whereSql,
          request.getSlice().getOffset() + 1,
          request.getSlice().getOffset() + request.getSlice().getSize() + 1);
    } else {
      String sql = StrUtil.format("SELECT {} FROM {}", selectSql, tableSQl);

      if (!SearchCriteria.TRUE.equalsIgnoreCase(whereSql)) {
        sql = StrUtil.format("{} WHERE {}", sql, whereSql);
      }

      if (!ObjectUtil.isEmpty(orderBySql)) {
        sql = StrUtil.format("{} {}", sql, orderBySql);
      }

      String limitSql = prepareLimit(request);
      if (!ObjectUtil.isEmpty(limitSql)) {
        sql = StrUtil.format("{} {}", sql, limitSql);
      }
      return sql;
    }
  }

  private void ensureOrderByForPartition(SearchRequest<T> request) {
    OrderBys orderBy = request.getOrderBy();
    if (orderBy.isEmpty()) {
      orderBy.addOrderBy(new OrderBy(ID));
    }
  }

  public String leftJoinTables(UserContext userContext, List<String> tables) {
    List<String> sortedTables = new ArrayList<>();
    for (String table : tables) {
      if (primaryTableNames.contains(table)) {
        sortedTables.add(table);
      }
    }
    for (String table : tables) {
      if (!primaryTableNames.contains(table)) {
        sortedTables.add(table);
      }
    }

    if (!userContext.getBool(MULTI_TABLE, false)) {
      return StrUtil.format("{}", sortedTables.get(0));
    }

    StringBuilder sb = new StringBuilder();
    String preTable = null;
    for (String sortedTable : sortedTables) {
      if (preTable == null) {
        preTable = sortedTable;
        sb.append(StrUtil.format("{} AS {}", sortedTable, tableAlias(sortedTable)));
        continue;
      }
      sb.append(
          StrUtil.format(
              " LEFT JOIN {} AS {} ON {}.{} = {}.{}",
              sortedTable,
              tableAlias(sortedTable),
              tableAlias(sortedTable),
              ID,
              tableAlias(preTable),
              ID));
    }
    return sb.toString();
  }

  private String collectSelectSql(
      UserContext userContext,
      SearchRequest request,
      String idTable,
      Map<String, Object> pParameters) {
    List<SimpleNamedExpression> allSelects = new ArrayList<>();
    List<SimpleNamedExpression> projections = request.getProjections();
    if (projections != null) {
      allSelects.addAll(projections);
    }
    List<SimpleNamedExpression> simpleDynamicProperties = request.getSimpleDynamicProperties();
    if (simpleDynamicProperties != null) {
      allSelects.addAll(simpleDynamicProperties);
    }
    return allSelects.stream()
        .map(e -> ExpressionHelper.toSql(userContext, e, idTable, pParameters, this))
        .collect(Collectors.joining(", "));
  }

  private List<String> collectDataTables(UserContext userContext, SearchRequest<T> request) {
    List<String> allRelationProperties = request.dataProperties(userContext);
    return collectTablesFromProperties(userContext, allRelationProperties);
  }

  private ArrayList<String> collectTablesFromProperties(
      UserContext userContext, List<String> properties) {
    Set<String> tables = new HashSet<>();
    for (String target : properties) {
      PropertyDescriptor property = findProperty(target);
      if (property.isId()) {
        continue;
      }
      List<SQLColumn> sqlColumns = getSqlColumns(property);
      for (SQLColumn sqlColumn : sqlColumns) {
        tables.add(sqlColumn.getTableName());
      }
    }
    return ListUtil.toList(tables);
  }

  private String tableAlias(String table) {
    return NamingCase.toCamelCase(table);
  }

  protected String prepareLimit(SearchRequest request) {
    Slice slice = request.getSlice();
    if (ObjectUtil.isEmpty(slice)) {
      return null;
    }
    return StrUtil.format("LIMIT {} OFFSET {}", slice.getSize(), slice.getOffset());
  }

  private String prepareOrderBy(
      UserContext userContext,
      SearchRequest request,
      String idTable,
      Map<String, Object> parameters) {
    OrderBys orderBys = request.getOrderBy();

    if (ObjectUtil.isEmpty(orderBys)) {
      return null;
    }
    return ExpressionHelper.toSql(userContext, orderBys, idTable, parameters, this);
  }

  private String prepareCondition(
      UserContext userContext,
      String idTable,
      SearchCriteria searchCriteria,
      Map<String, Object> parameters) {
    if (ObjectUtil.isEmpty(searchCriteria)) {
      return SearchCriteria.TRUE;
    }
    return ExpressionHelper.toSql(userContext, searchCriteria, idTable, parameters, this);
  }

  public boolean isRequestInDatasource(UserContext pUserContext, Repository repository) {
    if (repository instanceof SQLRepository) {
      return this.dataSource == ((SQLRepository<?>) repository).dataSource;
    }
    return false;
  }

  public String tableName(String type) {
    return NamingCase.toUnderlineCase(type + "_data");
  }

  public void ensureSchema(UserContext ctx) {
    List<SQLColumn> allColumns = new ArrayList<>();
    List<PropertyDescriptor> ownProperties = entityDescriptor.getOwnProperties();
    for (PropertyDescriptor ownProperty : ownProperties) {
      List<SQLColumn> sqlColumns = getSqlColumns(ownProperty);
      allColumns.addAll(sqlColumns);
    }
    if (entityDescriptor.hasChildren()) {
      SQLColumn childTypeCell = new SQLColumn(thisPrimaryTableName, getChildType());
      childTypeCell.setType(getChildSqlType());
      allColumns.add(childTypeCell);
    }
    Map<String, List<SQLColumn>> tableColumns =
        CollStreamUtil.groupByKey(allColumns, SQLColumn::getTableName);
    tableColumns.forEach(
        (table, columns) -> {
          String sql = findTableColumnsSql(dataSource, table);
          List<Map<String, Object>> dbTableInfo;
          try {
            dbTableInfo = jdbcTemplate.queryForList(sql, Collections.emptyMap());
          } catch (Exception exception) {
            dbTableInfo = ListUtil.empty();
          }
          ensure(ctx, dbTableInfo, table, columns);
        });
    ensureInitData(ctx);
    ensureIndexAndForeignKey(ctx);
    ensureIdSpaceTable(ctx);
  }

  protected String findTableColumnsSql(DataSource dataSource, String table) {
    return String.format("select * from information_schema.columns where table_name = '%s'", table);
  }

  protected void ensureIdSpaceTable(UserContext ctx) {
    String sql = findIdSpaceTableSql();
    List<Map<String, Object>> dbTableInfo;
    try {
      dbTableInfo = jdbcTemplate.queryForList(sql, Collections.emptyMap());
    } catch (Exception exception) {
      dbTableInfo = ListUtil.empty();
    }

    if (!ObjectUtil.isEmpty(dbTableInfo)) {
      return;
    }

    String createIdSpaceSql = getIdSpaceSql();
    ctx.info(createIdSpaceSql + ";");
    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      try {
        jdbcTemplate.getJdbcTemplate().execute(createIdSpaceSql);
      } catch (DataAccessException pE) {
        throw new RepositoryException(pE);
      }
    }
  }

  public String getIdSpaceSql() {
    StringBuilder sb = new StringBuilder();
    sb.append("CREATE TABLE ")
        .append(getTqlIdSpaceTable())
        .append(" (\n")
        .append("type_name varchar(100) PRIMARY KEY,\n")
        .append("current_level bigint)\n");
    String createIdSpaceSql = sb.toString();
    return createIdSpaceSql;
  }

  protected String findIdSpaceTableSql() {
    return findTableColumnsSql(dataSource, getTqlIdSpaceTable());
  }

  private Map<String, Object> getOneRow(ResultSet rs, ResultSetMetaData metaData)
      throws SQLException {
    Map<String, Object> oneRow = new HashMap<>();
    for (int i = 0; i < metaData.getColumnCount(); i++) {
      String columnName = metaData.getColumnName(i + 1);
      Object value = rs.getObject(i + 1);
      oneRow.put(columnName, value);
    }
    return oneRow;
  }

  @Override
  public Long prepareId(UserContext userContext, T entity) {
    if (entity.getId() != null) {
      return entity.getId();
    }
    Long id = userContext.generateId(entity);
    if (id != null) {
      return id;
    }

    AtomicLong current = new AtomicLong();
    try {
      final DataSourceTransactionManager transactionManager =
          new DataSourceTransactionManager(dataSource);
      TransactionTemplate transactionTemplate = new TransactionTemplate(transactionManager);
      transactionTemplate.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
      transactionTemplate.executeWithoutResult(
          tx -> {
            String type = CollectionUtil.getLast(types);
            Number dbCurrent = null;
            try {
              dbCurrent =
                  jdbcTemplate.queryForObject(
                      StrUtil.format(
                          "SELECT current_level from {} WHERE type_name = '{}' for update",
                          getTqlIdSpaceTable(),
                          type),
                      Collections.emptyMap(),
                      Long.class);
            } catch (Exception e) {

            }

            if (dbCurrent == null) {
              current.set(1l);
              jdbcTemplate
                  .getJdbcTemplate()
                  .execute(
                      StrUtil.format(
                          "INSERT INTO {} VALUES ('{}', {})", getTqlIdSpaceTable(), type, current));
              return;
            }
            dbCurrent = NumberUtil.add(dbCurrent, 1);
            jdbcTemplate
                .getJdbcTemplate()
                .execute(
                    StrUtil.format(
                        "UPDATE {} SET current_level = {} WHERE type_name = '{}'",
                        getTqlIdSpaceTable(),
                        dbCurrent,
                        type));
            current.set(dbCurrent.longValue());
          });
    } catch (Exception pE) {
      throw new RepositoryException(pE);
    }
    return current.get();
  }

  private void ensureIndexAndForeignKey(UserContext ctx) {}

  public void ensureInitData(UserContext ctx) {
    if (entityDescriptor.isRoot()) {
      ensureRoot(ctx);
    }
    if (entityDescriptor.isConstant()) {
      ensureConstant(ctx);
    }
  }

  private void ensureConstant(UserContext ctx) {
    PropertyDescriptor identifier = entityDescriptor.getIdentifier();
    List<String> candidates = identifier.getCandidates();
    List<PropertyDescriptor> ownProperties = entityDescriptor.getOwnProperties();
    List<String> columns = new ArrayList<>();
    for (PropertyDescriptor ownProperty : ownProperties) {
      SQLColumn sqlColumn = getSqlColumn(ownProperty);
      columns.add(sqlColumn.getColumnName());
    }
    for (int i = 0; i < candidates.size(); i++) {
      String code = candidates.get(i);
      List oneConstant = new ArrayList();
      for (PropertyDescriptor ownProperty : ownProperties) {
        oneConstant.add(getConstantPropertyValue(ctx, ownProperty, i, code));
      }

      Map<String, Object> dbRootRow = null;
      try {
        dbRootRow =
            jdbcTemplate.queryForMap(
                StrUtil.format(
                    "SELECT * FROM {} WHERE id = {}",
                    tableName(entityDescriptor.getType()),
                    genIdForCandidateCode(code)),
                Collections.emptyMap());
      } catch (Exception e) {

      }

      if (dbRootRow != null) {
        long version = ((Number) dbRootRow.get("version")).longValue();
        if (version > 0) {
          return;
        }
        // update version
        String sql =
            StrUtil.format(
                "UPDATE {} SET version = {} where id = '{}'",
                tableName(entityDescriptor.getType()),
                -version,
                genIdForCandidateCode(code));
        ctx.info(sql + ";");
        if (ctx.config() != null && ctx.config().isEnsureTable()) {
          try {
            jdbcTemplate.getJdbcTemplate().execute(sql);
          } catch (DataAccessException pE) {
            throw new RepositoryException(pE);
          }
        }
        return;
      }

      String sql =
          StrUtil.format(
              "INSERT INTO {} ({}) VALUES ({})",
              tableName(entityDescriptor.getType()),
              CollectionUtil.join(columns, ","),
              CollectionUtil.join(oneConstant, ",", value -> getSqlValue(value)));
      ctx.info(sql + ";");
      if (ctx.config() != null && ctx.config().isEnsureTable()) {
        try {
          jdbcTemplate.getJdbcTemplate().execute(sql);
        } catch (DataAccessException pE) {
          throw new RepositoryException(pE);
        }
      }
    }
  }

  private long genIdForCandidateCode(String code) {
    return Math.abs(code.toUpperCase().hashCode());
  }

  private Object getConstantPropertyValue(
      UserContext ctx, PropertyDescriptor property, int index, String identifier) {
    if (property.isId()) {
      return genIdForCandidateCode(identifier);
    }
    if (property.isVersion()) {
      return 1l;
    }

    PropertyType type = property.getType();
    if (BaseEntity.class.isAssignableFrom(type.javaType())) {
      String referType = type.javaType().getSimpleName();
      EntityDescriptor refer = ctx.resolveEntityDescriptor(referType);
      if (refer.isRoot()) {
        return "1";
      }
      // set others as null
      return null;
    }

    String createFunction = property.getAdditionalInfo().get("createFunction");
    if (!ObjectUtil.isEmpty(createFunction)) {
      return ReflectUtil.invoke(ctx, createFunction);
    }

    List<String> candidates = property.getCandidates();

    if (property.isIdentifier()) {
      return NamingCase.toPascalCase(identifier);
    }

    return CollectionUtil.get(candidates, index);
  }

  private void ensureRoot(UserContext ctx) {
    Map<String, Object> dbRootRow = null;
    try {
      dbRootRow =
          jdbcTemplate.queryForMap(
              StrUtil.format(
                  "SELECT * FROM {} WHERE id = 1", tableName(entityDescriptor.getType())),
              Collections.emptyMap());
    } catch (Exception e) {

    }

    if (dbRootRow != null) {
      long version = ((Number) dbRootRow.get("version")).longValue();
      if (version > 0) {
        return;
      }
      // update version
      String sql =
          StrUtil.format(
              "UPDATE {} SET version = {} where id = '1'\n",
              tableName(entityDescriptor.getType()),
              -version);
      ctx.info(sql + ";");
      if (ctx.config() != null && ctx.config().isEnsureTable()) {
        try {
          jdbcTemplate.getJdbcTemplate().execute(sql);
        } catch (DataAccessException pE) {
          throw new RepositoryException(pE);
        }
      }
      return;
    }
    List<String> columns = new ArrayList();
    List rootRow = new ArrayList();
    List<PropertyDescriptor> ownProperties = entityDescriptor.getOwnProperties();
    for (PropertyDescriptor ownProperty : ownProperties) {
      Object value = getRootPropertyValue(ctx, ownProperty);
      rootRow.add(value);
      SQLColumn sqlColumn = getSqlColumn(ownProperty);
      columns.add(sqlColumn.getColumnName());
    }
    String sql =
        StrUtil.format(
            "INSERT INTO {} ({}) VALUES ({})\n",
            tableName(entityDescriptor.getType()),
            CollectionUtil.join(columns, ","),
            CollectionUtil.join(rootRow, ",", value -> getSqlValue(value)));
    ctx.info(sql + ";");
    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      try {
        jdbcTemplate.getJdbcTemplate().execute(sql);
      } catch (DataAccessException pE) {
        throw new RepositoryException(pE);
      }
    }
  }

  protected String getSqlValue(Object value) {
    return StrUtil.wrapIfMissing(String.valueOf(value), "'", "'");
  }

  private Object getRootPropertyValue(UserContext ctx, PropertyDescriptor property) {
    if (property.isId()) {
      return 1l;
    }
    if (property.isVersion()) {
      return 1l;
    }
    String createFunction = property.getAdditionalInfo().get("createFunction");
    if (!ObjectUtil.isEmpty(createFunction)) {
      return ReflectUtil.invoke(ctx, createFunction);
    }
    return property.getAdditionalInfo().get("candidates");
  }

  protected void ensure(
      UserContext ctx, List<Map<String, Object>> tableInfo, String table, List<SQLColumn> columns) {
    // 表格不存在
    if (tableInfo.isEmpty()) {
      createTable(ctx, table, columns);
      return;
    }

    Map<String, Map<String, Object>> fields =
        CollStreamUtil.toIdentityMap(tableInfo, m -> String.valueOf(m.get("column_name")));

    for (int i = 0; i < columns.size(); i++) {
      SQLColumn column = columns.get(i);
      String tableName = column.getTableName();
      String columnName = column.getColumnName();
      String type = column.getType();

      String preColumnName = null;
      if (i > 0) {
        preColumnName = columns.get(i - 1).getColumnName();
      }
      String dbColumnName = getPureColumnName(columnName);
      Map<String, Object> field = fields.get(dbColumnName);
      if (field == null) {
        addColumn(ctx, tableName, preColumnName, columnName, type);
        continue;
      }

      String dbType = calculateDBType(field);
      if (dbType.equalsIgnoreCase(type)) {
        continue;
      }

      alterColumn(ctx, tableName, columnName, type);
    }
  }

  protected String getPureColumnName(String columnName) {
    return StrUtil.unWrap(columnName, '\"');
  }

  protected String calculateDBType(Map<String, Object> columnInfo) {
    String dataType = (String) columnInfo.get("data_type");
    switch (dataType) {
      case "bigint":
        return "bigint";
      case "tinyint":
      case "boolean":
        return "boolean";
      case "varchar":
      case "character varying":
        return StrUtil.format("varchar({})", columnInfo.get("character_maximum_length"));
      case "date":
        return "date";
      case "int":
      case "integer":
        return "integer";
      case "decimal":
      case "numeric":
        return StrUtil.format(
            "numeric({},{})", columnInfo.get("numeric_precision"), columnInfo.get("numeric_scale"));
      case "text":
        return "text";
      case "time without time zone":
        return "time";
      case "timestamp":
      case "timestamp without time zone":
        return "timestamp";
      default:
        throw new RepositoryException("未处理的类型:" + dataType);
    }
  }

  protected void alterColumn(UserContext ctx, String tableName, String columnName, String type) {
    String alterColumnSql =
        StrUtil.format("ALTER TABLE {} ALTER COLUMN {} TYPE {}", tableName, columnName, type);
    ctx.info(alterColumnSql + ";");
    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      try {
        jdbcTemplate.getJdbcTemplate().execute(alterColumnSql);
      } catch (DataAccessException pE) {
        throw new RepositoryException(pE);
      }
    }
  }

  private void addColumn(
      UserContext ctx, String tableName, String preColumnName, String columnName, String type) {
    String addColumnSql =
        StrUtil.format("ALTER TABLE {} ADD COLUMN {} {}", tableName, columnName, type);
    ctx.info(addColumnSql + ";");
    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      try {
        jdbcTemplate.getJdbcTemplate().execute(addColumnSql);
      } catch (DataAccessException pE) {
        throw new RepositoryException(pE);
      }
    }
  }

  private void createTable(UserContext ctx, String table, List<SQLColumn> columns) {
    StringBuilder sb = new StringBuilder();
    sb.append("CREATE TABLE ").append(table).append(" (\n");
    sb.append(
        columns.stream()
            .map(
                column -> {
                  String dbColumn = column.getColumnName() + " " + column.getType();
                  if ("id".equalsIgnoreCase(column.getColumnName())) {
                    dbColumn = dbColumn + " PRIMARY KEY";
                  }
                  return dbColumn;
                })
            .collect(Collectors.joining(",\n")));
    sb.append(")\n");
    String createTableSql = sb.toString();
    ctx.info(createTableSql + ";");

    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      try {
        jdbcTemplate.getJdbcTemplate().execute(createTableSql);
      } catch (DataAccessException pE) {
        throw new RepositoryException(pE);
      }
    }
  }

  @Override
  public List<SQLColumn> getPropertyColumns(String idTable, String propertyName) {
    if (getChildType().equalsIgnoreCase(propertyName)) {
      if (entityDescriptor.hasChildren()) {
        SQLColumn sqlColumn = new SQLColumn(tableAlias(thisPrimaryTableName), getChildType());
        sqlColumn.setType(getChildSqlType());
        return ListUtil.of(sqlColumn);
      } else {
        return ListUtil.empty();
      }
    }

    PropertyDescriptor property = findProperty(propertyName);
    List<SQLColumn> sqlColumns = getSqlColumns(property);
    for (SQLColumn sqlColumn : sqlColumns) {
      if (property.isId()) {
        sqlColumn.setTableName(tableAlias(idTable));
      } else {
        sqlColumn.setTableName(tableAlias(sqlColumn.getTableName()));
      }
    }
    return sqlColumns;
  }

  public String getChildType() {
    return childType;
  }

  public void setChildType(String pChildType) {
    childType = pChildType;
  }

  public String getChildSqlType() {
    return childSqlType;
  }

  public void setChildSqlType(String pChildSqlType) {
    childSqlType = pChildSqlType;
  }

  public String getTqlIdSpaceTable() {
    return tqlIdSpaceTable;
  }

  public void setTqlIdSpaceTable(String pTqlIdSpaceTable) {
    tqlIdSpaceTable = pTqlIdSpaceTable;
  }

  public Map<Class, SQLExpressionParser> getExpressionParsers() {
    return expressionParsers;
  }
}
