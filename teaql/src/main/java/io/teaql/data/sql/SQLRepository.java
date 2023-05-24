package io.teaql.data.sql;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.comparator.CompareUtil;
import cn.hutool.core.text.NamingCase;
import cn.hutool.core.util.*;
import io.teaql.data.*;
import io.teaql.data.criteria.EQ;
import io.teaql.data.criteria.IN;
import io.teaql.data.criteria.TwoOperatorCriteria;
import io.teaql.data.event.EntityCreatedEvent;
import io.teaql.data.event.EntityDeletedEvent;
import io.teaql.data.event.EntityRecoverEvent;
import io.teaql.data.event.EntityUpdatedEvent;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.PropertyType;
import io.teaql.data.meta.Relation;
import io.teaql.data.sql.expression.ExpressionHelper;
import java.sql.ResultSet;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import javax.sql.DataSource;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.transaction.support.TransactionTemplate;

public class SQLRepository<T extends Entity> implements Repository<T>, SQLColumnResolver {
  public static final String VERSION = "version";
  public static final String ID = "id";
  public static final String CHILD_TYPE = "_child_type";
  public static final String CHILD_SQL_TYPE = "VARCHAR(100)";

  public static final String MULTI_TABLE = "MULTI_TABLE";
  public static final String TEAQL_ID_SPACE_TABLE = "teaql_id_space";

  private final EntityDescriptor entityDescriptor;
  private final NamedParameterJdbcTemplate jdbcTemplate;
  private String versionTableName;
  private List<String> primaryTableNames = new ArrayList<>();
  private String thisPrimaryTableName;
  private Set<String> allTableNames = new LinkedHashSet<>();
  private List<String> types = new ArrayList<>();
  private List<String> auxiliaryTableNames;
  private List<PropertyDescriptor> allProperties = new ArrayList<>();

  public SQLRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    this.entityDescriptor = entityDescriptor;
    jdbcTemplate = new NamedParameterJdbcTemplate(dataSource);
    initSQLMeta(entityDescriptor);
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

  @Override
  public Collection<T> save(UserContext userContext, Collection<T> entities) {
    if (ObjectUtil.isEmpty(entities)) {
      return entities;
    }
    Collection<T> newItems = CollUtil.filterNew(entities, Entity::newItem);
    if (ObjectUtil.isNotEmpty(newItems)) {
      createItems(userContext, newItems);
    }
    Collection<T> updatedItems = CollUtil.filterNew(entities, Entity::updateItem);
    if (ObjectUtil.isNotEmpty(updatedItems)) {
      updateItems(userContext, updatedItems);
    }
    Collection<T> deleteItems = CollUtil.filterNew(entities, Entity::deleteItem);
    if (ObjectUtil.isNotEmpty(deleteItems)) {
      delete(userContext, deleteItems);
    }

    Collection<T> recoverItems = CollUtil.filterNew(entities, Entity::recoverItem);
    if (ObjectUtil.isNotEmpty(recoverItems)) {
      recover(userContext, recoverItems);
    }
    return entities;
  }

  private void updateItems(UserContext userContext, Collection<T> updateItems) {
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
      // 一个一个更新，不作批量
      Map<String, List<String>> tableColumnNames = sqlEntity.getTableColumnNames();
      Map<String, List> tableColumnValues = sqlEntity.getTableColumnValues();

      // 用于标记version表是否已更新
      AtomicBoolean versionTableUpdated = new AtomicBoolean(false);
      tableColumnValues.forEach(
          (k, v) -> {
            // 此表需要更新的列
            List<String> columns = new ArrayList<>(tableColumnNames.get(k));

            // 此表对应的参数列表
            List l = new ArrayList(v);

            // version 表，通过id, version来作修改
            boolean versionTable = this.versionTableName.equals(k);

            // 主表，通过Id 来修改， 附属表（先查询是否有记录，有则直接更新，否则新增）
            boolean primaryTable = this.primaryTableNames.contains(k);
            if (versionTable) {
              updateVersionTable(userContext, sqlEntity, versionTableUpdated, k, columns, l);
            } else if (primaryTable) {
              updatePrimaryTable(userContext, sqlEntity, k, columns, l);
            } else {
              // 附属表，不检查结果
              jdbcTemplate
                  .getJdbcTemplate()
                  .update(
                      StrUtil.format(
                          "REPLACE INTO {} SET {}",
                          k,
                          columns.stream().map(c -> c + " = ?").collect(Collectors.joining(" , "))),
                      l.toArray(new Object[0]));
            }
          });

      // 如果没有更新version table属性，此处我们只更新version table的版本
      if (!versionTableUpdated.get()) {
        updateVersionTableVersion(userContext, sqlEntity);
      }
    }
    for (T updateItem : updateItems) {
      updateItem.setVersion(updateItem.getVersion() + 1);
      if (updateItem instanceof BaseEntity item) {
        userContext.sendEvent(new EntityUpdatedEvent(item));
        item.gotoNextStatus(EntityAction.PERSIST);
        userContext.afterPersist(item);
      }
    }
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
    int update = jdbcTemplate.getJdbcTemplate().update(updateSql, parameters);
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
    int update = jdbcTemplate.getJdbcTemplate().update(updateSql, parameters);
    SQLLogger.logSQLAndParameters(userContext, updateSql, parameters, update + " UPDATED");
    if (update != 1) {
      throw new RepositoryException("主表数据更新失败");
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
    int update = jdbcTemplate.getJdbcTemplate().update(updateSql, parameters);
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

  private void createItems(UserContext userContext, Collection<T> createItems) {
    List<SQLEntity> sqlEntities =
        CollectionUtil.map(createItems, i -> convertToSQLEntityForInsert(userContext, i), true);
    if (ObjectUtil.isEmpty(sqlEntities)) {
      return;
    }

    // 插入时所有的结构应该是一样的，取第一个为模板
    SQLEntity sqlEntity = sqlEntities.get(0);
    Map<String, List<String>> tableColumns = sqlEntity.getTableColumnNames();

    // 插入时批量操作，先收集所有的行
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
            // 附属表, 如果这一行全是Null值，跳过插入
            if (auxiliaryTableNames.contains(k) && entity.allNullExceptID(v)) {
              return;
            }
            values.add(v.toArray(new Object[0]));
          });
    }

    rows.forEach(
        (k, v) -> {
          // 此表没有数据需要插入，跳过
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
          int[] rets = jdbcTemplate.getJdbcTemplate().batchUpdate(sql, v);
          int i = 0;
          for (int ret : rets) {
            SQLLogger.logSQLAndParameters(userContext, sql, v.get(i++), ret + " UPDATED");
          }
        });

    for (T createItem : createItems) {
      if (createItem instanceof BaseEntity item) {
        item.gotoNextStatus(EntityAction.PERSIST);
        userContext.sendEvent(new EntityCreatedEvent(item));
        userContext.afterPersist(item);
      }
    }
  }

  private SQLEntity convertToSQLEntityForInsert(UserContext userContext, T entity) {
    // insert时确保id存在，version为1
    setIdAndVersionForInsert(userContext, entity);

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
      childTypeCell.setColumnName(CHILD_TYPE);
      childTypeCell.setValue(type);
      sqlEntity.addPropertySQLData(childTypeCell);
    }

    return sqlEntity;
  }

  private boolean shouldHandle(Relation propertyDescriptor) {
    EntityDescriptor relationKeeper = propertyDescriptor.getRelationKeeper();

    EntityDescriptor entityDescriptor = this.entityDescriptor;
    while (entityDescriptor != null) {
      if (entityDescriptor == relationKeeper) {
        return true;
      }
      entityDescriptor = entityDescriptor.getParent();
    }
    return false;
  }

  private void setIdAndVersionForInsert(UserContext userContext, Entity entity) {
    Long id = prepareId(userContext, (T) entity);
    entity.setId(id);
    entity.setVersion(1L);
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
  public void delete(UserContext userContext, Collection<T> entities) {
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
    int[] rets = jdbcTemplate.getJdbcTemplate().batchUpdate(updateSql, args);
    int i = 0;
    for (int ret : rets) {
      SQLLogger.logSQLAndParameters(userContext, updateSql, args.get(i++), ret + " UPDATED");
      if (ret != 1) {
        throw new ConcurrentModifyException();
      }
    }

    for (T deleteItem : entities) {
      deleteItem.setVersion(-(deleteItem.getVersion() + 1));
      if (deleteItem instanceof BaseEntity item) {
        item.gotoNextStatus(EntityAction.PERSIST);
        userContext.sendEvent(new EntityDeletedEvent(item));
        userContext.afterPersist(item);
      }
    }
  }

  @Override
  public void recover(UserContext userContext, Collection<T> entities) {
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
    int[] rets = jdbcTemplate.getJdbcTemplate().batchUpdate(updateSql, args);
    int i = 0;
    for (int ret : rets) {
      SQLLogger.logSQLAndParameters(userContext, updateSql, args.get(i++), ret + " UPDATED");
      if (ret != 1) {
        throw new ConcurrentModifyException();
      }
    }
    for (T recoverItem : entities) {
      recoverItem.setVersion(-recoverItem.getVersion() + 1);
      if (recoverItem instanceof BaseEntity item) {
        item.gotoNextStatus(EntityAction.PERSIST);
        userContext.sendEvent(new EntityRecoverEvent(item));
        userContext.afterPersist(item);
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

  @Override
  public T execute(UserContext userContext, SearchRequest<T> request) {
    if (request instanceof BaseRequest) {
      ((BaseRequest<T>) request).top(1);
    }
    return executeForList(userContext, request).first();
  }

  @Override
  public SmartList<T> executeForList(UserContext userContext, SearchRequest<T> request) {
    Map<String, Object> params = new HashMap<>();

    // 准备sql，以及参数
    String sql = buildDataSQL(userContext, request, params);
    List<T> results = new ArrayList<>();
    if (!ObjectUtil.isEmpty(sql)) {
      results = this.jdbcTemplate.query(sql, params, getMapper(userContext, request));
      SQLLogger.logNamedSQL(userContext, sql, params, results);
    }
    SmartList<T> smartList = new SmartList<>(results);
    enhanceRelations(userContext, smartList, request);
    enhanceWithAggregation(userContext, request, smartList);
    addDynamicAggregations(userContext, request, smartList);
    return smartList;
  }

  private void addDynamicAggregations(
      UserContext userContext, SearchRequest<T> request, SmartList<T> results) {
    List<SimpleAggregation> dynamicAggregateAttributes = request.getDynamicAggregateAttributes();
    if (ObjectUtil.isEmpty(dynamicAggregateAttributes)) {
      return;
    }

    Map<Long, T> idEntityMap = results.mapById();
    Set<Long> ids = idEntityMap.keySet();
    if (ObjectUtil.isEmpty(ids)) {
      return;
    }

    for (SimpleAggregation dynamicAggregateAttribute : dynamicAggregateAttributes) {
      SearchRequest aggregateRequest = dynamicAggregateAttribute.getAggregateRequest();
      String property = aggregateRequest.getPartitionProperty();
      TempRequest t = new TempRequest(aggregateRequest);
      t.groupBy(property);
      if (ids.size() < preferIdInCount()) {
        t.appendSearchCriteria(
            new IN(new PropertyReference(property), new Parameter(property, ids)));
      } else {
        t.appendSearchCriteria(new SubQuerySearchCriteria(property, request, ID));
      }
      List<SearchRequest> aggregations = findAggregations(userContext, t);
      SearchRequest aggregatePoint = aggregations.get(0);
      AggregationResult aggregation = aggregatePoint.aggregation(userContext);
      if (dynamicAggregateAttribute.isSingleNumber()) {
        saveSingleDynamicValue(idEntityMap, dynamicAggregateAttribute, aggregation);
      } else {
        List<Map<String, Object>> dynamicAttributes = aggregation.toList();
        for (Map<String, Object> dynamicAttribute : dynamicAttributes) {
          saveMultiDynamicValue(idEntityMap, dynamicAggregateAttribute, property, dynamicAttribute);
        }
      }
    }
  }

  private void saveMultiDynamicValue(
      Map<Long, T> idEntityMap,
      SimpleAggregation dynamicAggregateAttribute,
      String property,
      Map<String, Object> dynamicAttribute) {
    Long parentID = ((Number) dynamicAttribute.remove(property)).longValue();
    T parent = idEntityMap.get(parentID);
    parent.appendDynamicProperty(dynamicAggregateAttribute.getName(), dynamicAttribute);
  }

  private void saveSingleDynamicValue(
      Map<Long, T> idEntityMap,
      SimpleAggregation dynamicAggregateAttribute,
      AggregationResult aggregation) {
    Map<Object, Number> simpleMap = aggregation.toSimpleMap();
    simpleMap.forEach(
        (parentId, value) -> {
          T parent = idEntityMap.get(parentId);
          parent.addDynamicProperty(dynamicAggregateAttribute.getName(), value);
        });
  }

  private int preferIdInCount() {
    return 1000;
  }

  @Override
  public AggregationResult aggregation(UserContext userContext, SearchRequest<T> request) {
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
    List<AggregationItem> aggregationItems =
        jdbcTemplate.query(sql, parameters, getAggregationMapper(request));
    AggregationResult result = new AggregationResult();
    result.setName(request.getAggregations().getName());
    result.setData(aggregationItems);
    advanceGroupBy(userContext, result, request);
    return result;
  }

  private void advanceGroupBy(
      UserContext userContext, AggregationResult result, SearchRequest<T> request) {
    Map<String, SearchRequest> propagateDimensions = request.getPropagateDimensions();
    List<SimpleNamedExpression> allDimensions = request.getAggregations().getDimensions();
    propagateDimensions.forEach(
        (property, dimensionRequest) -> {
          SimpleNamedExpression toBeEnhancedDimension =
              findCurrentDimension(allDimensions, property);

          List propagateDimensionValues = result.getPropagateDimensionValues(property);
          TempRequest t =
              new TempRequest(dimensionRequest.returnType(), dimensionRequest.getTypeName());

          // 自身的条件
          t.appendSearchCriteria(dimensionRequest.getSearchCriteria());

          // 动态属性查询(ID关联)
          t.addSimpleDynamicProperty(property, new PropertyReference(ID));

          Map<String, SearchRequest> subPropagateDimensions =
              dimensionRequest.getPropagateDimensions();
          subPropagateDimensions.forEach(
              (k, v) -> {
                t.groupBy(k, v);
              });

          // 更多的dimension关联
          List<SimpleNamedExpression> dimensions =
              dimensionRequest.getAggregations().getDimensions();
          for (SimpleNamedExpression dimension : dimensions) {
            t.addSimpleDynamicProperty(dimension.name(), dimension.getExpression());
          }
          t.appendSearchCriteria(
              new IN(new PropertyReference(ID), new Parameter(ID, propagateDimensionValues)));
          SmartList orderByResults = t.executeForList(userContext);
          appendResult(userContext, result, t, toBeEnhancedDimension, orderByResults);
        });
  }

  private SimpleNamedExpression findCurrentDimension(
      List<SimpleNamedExpression> allDimensions, String property) {
    for (SimpleNamedExpression dimension : allDimensions) {
      if (dimension.name().equals(property)) {
        return dimension;
      }
    }
    return null;
  }

  private void appendResult(
      UserContext userContext,
      AggregationResult result,
      SearchRequest dimensionRequest,
      SimpleNamedExpression toBeRefinedDimension,
      SmartList<Entity> dimensionResult) {
    List<SimpleNamedExpression> simpleDynamicProperties =
        dimensionRequest.getSimpleDynamicProperties();

    Map<Object, Map<SimpleNamedExpression, Object>> refinedDimensions =
        CollStreamUtil.toMap(
            dimensionResult.getData(),
            e -> e.getDynamicProperty(toBeRefinedDimension.name()),
            e -> {
              Map<SimpleNamedExpression, Object> refinedDimension = new HashMap<>();
              for (SimpleNamedExpression simpleDynamicProperty : simpleDynamicProperties) {
                if (simpleDynamicProperty.name().equals(toBeRefinedDimension.name())) {
                  continue;
                }
                refinedDimension.put(
                    simpleDynamicProperty, e.getDynamicProperty(simpleDynamicProperty.name()));
              }
              return refinedDimension;
            });

    List<AggregationItem> data = result.getData();

    for (AggregationItem datum : data) {
      Map<SimpleNamedExpression, Object> dimensions = datum.getDimensions();
      Object currentValue = remove(dimensions, toBeRefinedDimension);
      if (currentValue == null) {
        continue;
      }
      Map<SimpleNamedExpression, Object> replacements = refinedDimensions.get(currentValue);
      if (replacements != null) {
        dimensions.putAll(replacements);
      }
    }

    // merge
    Map<Map<SimpleNamedExpression, Object>, AggregationItem> collect =
        data.stream()
            .collect(
                Collectors.toMap(
                    item -> item.getDimensions(),
                    item -> item,
                    (pre, current) -> {
                      Map<SimpleNamedExpression, Object> preValues = pre.getValues();
                      Map<SimpleNamedExpression, Object> currentValues = current.getValues();
                      Set<SimpleNamedExpression> simpleNamedExpressions = preValues.keySet();
                      for (SimpleNamedExpression simpleNamedExpression : simpleNamedExpressions) {
                        preValues.put(
                            simpleNamedExpression,
                            merge(
                                simpleNamedExpression,
                                preValues.get(simpleNamedExpression),
                                currentValues.get(simpleNamedExpression)));
                      }
                      return pre;
                    }));

    advanceGroupBy(userContext, result, dimensionRequest);
  }

  private Object remove(
      Map<SimpleNamedExpression, Object> dimensions, SimpleNamedExpression toBeRefinedDimension) {
    Set<Map.Entry<SimpleNamedExpression, Object>> entries = dimensions.entrySet();
    Iterator<Map.Entry<SimpleNamedExpression, Object>> iterator = entries.iterator();
    while (iterator.hasNext()) {
      Map.Entry<SimpleNamedExpression, Object> next = iterator.next();
      SimpleNamedExpression key = next.getKey();
      Object value = next.getValue();
      if (key.name().equals(toBeRefinedDimension.name())) {
        iterator.remove();
        return value;
      }
    }
    return null;
  }

  private Object merge(SimpleNamedExpression aggregation, Object p0, Object p1) {
    Expression expression = aggregation.getExpression();
    if (!(expression instanceof FunctionApply)) {
      throw new RepositoryException("目前聚合函数只能是FunctionApply表达式");
    }

    PropertyFunction operator = ((FunctionApply) expression).getOperator();
    if (!(operator instanceof AggrFunction)) {
      throw new RepositoryException("目前聚合函数只能是AggrFunction");
    }
    AggrFunction aggr = (AggrFunction) operator;
    if (aggr == AggrFunction.COUNT || aggr == AggrFunction.SUM) {
      return NumberUtil.add((Number) p0, (Number) p1);
    }

    if (aggr == AggrFunction.MIN) {
      return CompareUtil.compare((Comparable) p0, (Comparable) p1) < 0 ? p0 : p1;
    }

    if (aggr == AggrFunction.MAX) {
      return CompareUtil.compare((Comparable) p0, (Comparable) p1) < 0 ? p1 : p0;
    }

    throw new RepositoryException("不支持的AggrFunction" + aggr);
  }

  private RowMapper<AggregationItem> getAggregationMapper(SearchRequest<T> request) {
    return (rs, rowNum) -> {
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

  private void enhanceWithAggregation(
      UserContext userContext, SearchRequest<T> request, SmartList<T> results) {
    List<SearchRequest> aggregationRequests = findAggregations(userContext, request);
    for (SearchRequest aggregationRequest : aggregationRequests) {
      AggregationResult aggregation = aggregationRequest.aggregation(userContext);
      results.addAggregationResult(userContext, aggregation);
    }
  }

  private List<SearchRequest> findAggregations(UserContext userContext, SearchRequest request) {
    List<SearchRequest> ret = new ArrayList<>();
    if (request.hasSimpleAgg()) {
      ret.add(request);
    }
    Map<String, SearchRequest> propagateAggregations = request.getPropagateAggregations();
    propagateAggregations.forEach(
        (property, subRequest) -> {
          TempRequest t = new TempRequest(subRequest);
          PropertyDescriptor propertyDescriptor = findProperty(property);
          if (shouldHandle((Relation) propertyDescriptor)) {
            t.appendSearchCriteria(new SubQuerySearchCriteria(ID, request, property));
          } else {
            PropertyDescriptor reverseProperty =
                ((Relation) propertyDescriptor).getReverseProperty();
            t.appendSearchCriteria(
                new SubQuerySearchCriteria(reverseProperty.getName(), request, ID));
          }
          List<SearchRequest> aggregations = findAggregations(userContext, t);
          if (aggregations != null) {
            ret.addAll(aggregations);
          }
        });
    return ret;
  }

  private List<String> collectAggregationTables(UserContext userContext, SearchRequest<T> request) {
    return collectTablesFromProperties(userContext, request.aggregationProperties(userContext));
  }

  private void enhanceRelations(
      UserContext userContext, SmartList<T> results, SearchRequest<T> request) {
    if (results.isEmpty()) {
      return;
    }

    Map<String, SearchRequest> enhanceProperties = request.enhanceRelations();
    enhanceProperties.forEach(
        (p, r) -> {
          PropertyDescriptor property = findProperty(p);
          if (property == null) {
            return;
          }

          if (!(property instanceof Relation)) {
            return;
          }

          if (shouldHandle((Relation) property)) {
            enhanceParent(userContext, results, (Relation) property, r);
          } else {
            collectChildren(userContext, results, (Relation) property, r);
          }
        });
  }

  private void collectChildren(
      UserContext userContext,
      SmartList<T> results,
      Relation relation,
      SearchRequest childRequest) {
    if (ObjectUtil.isEmpty(results)) {
      return;
    }
    TempRequest childTempRequest = new TempRequest(childRequest);
    String typeName = childTempRequest.getTypeName();
    Repository repository = userContext.resolveRepository(typeName);
    PropertyDescriptor reverseProperty = relation.getReverseProperty();
    if (childTempRequest.getSlice() != null) {
      childTempRequest.setPartitionProperty(reverseProperty.getName());
    }

    childTempRequest.appendSearchCriteria(
        getSearchCriteriaOfCollectChildren(results, reverseProperty));

    SmartList children = repository.executeForList(userContext, childTempRequest);

    Map<Long, T> longTMap = results.mapById();
    for (Object child : children) {
      Entity childEntity = (Entity) child;
      Object parent = childEntity.getProperty(reverseProperty.getName());
      if (parent instanceof Entity) {
        T parentEntity = longTMap.get(((Entity) parent).getId());
        if (parentEntity != null) {
          parentEntity.addRelation(relation.getName(), childEntity);
        }
      }
    }
  }

  private TwoOperatorCriteria getSearchCriteriaOfCollectChildren(
      SmartList<T> results, PropertyDescriptor reverseProperty) {

    if (results.size() == 1) {
      return new EQ(
          new PropertyReference(reverseProperty.getName()),
          new Parameter(reverseProperty.getName(), results, false));
    }

    return new IN(
        new PropertyReference(reverseProperty.getName()),
        new Parameter(reverseProperty.getName(), results));
  }

  private void enhanceParent(
      UserContext userContext,
      SmartList<T> results,
      Relation relation,
      SearchRequest parentRequest) {
    if (ObjectUtil.isEmpty(results)) {
      return;
    }
    List<Entity> parents =
        results.stream()
            .map(e -> e.getProperty(relation.getName()))
            .filter(p -> p instanceof Entity)
            .map(e -> (Entity) e)
            .distinct()
            .toList();
    if (ObjectUtil.isEmpty(parents)) {
      return;
    }

    // parent request增加id约束
    TempRequest parentTemp = new TempRequest(parentRequest);
    parentTemp.appendSearchCriteria(new IN(new PropertyReference(ID), new Parameter(ID, parents)));
    Repository repository = userContext.resolveRepository(parentTemp.getTypeName());
    SmartList parentItems = repository.executeForList(userContext, parentTemp);

    Map map = parentItems.mapById();
    for (T result : results) {
      Object oldValue = result.getProperty(relation.getName());
      if (oldValue instanceof Entity) {
        result.addRelation(relation.getName(), (Entity) map.get(((Entity) oldValue).getId()));
      }
    }
  }

  private RowMapper<T> getMapper(UserContext pUserContext, SearchRequest<T> pRequest) {
    return (rs, rowNum) -> {
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

    // 收集所有相关联的表
    List<String> tables = collectDataTables(userContext, request);

    // 随机选择一个表（这里用了第一个）作为idTable
    if (ObjectUtil.isEmpty(tables)) {
      tables = new ArrayList<>(ListUtil.of(thisPrimaryTableName));
    }
    String idTable = tables.get(0);
    userContext.put(MULTI_TABLE, tables.size() > 1);

    // 生成查询条件
    String whereSql =
        prepareCondition(userContext, idTable, request.getSearchCriteria(), parameters);

    // 运算条件为false,不会有结果
    if (SearchCriteria.FALSE.equalsIgnoreCase(whereSql)) {
      return null;
    }

    // 分页要求不要数据(用于统计)
    if (request.getSlice() != null && request.getSlice().getSize() == 0) {
      return null;
    }

    String tableSQl = leftJoinTables(userContext, tables);

    // select部分
    String selectSql = collectSelectSql(userContext, request, idTable, parameters);

    String partitionProperty = request.getPartitionProperty();
    if (!ObjectUtil.isEmpty(partitionProperty) && request.getSlice() != null) {
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
    // table按主表排序
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
    StringBuilder sb = new StringBuilder();
    String preTable = null;
    for (String sortedTable : sortedTables) {
      if (preTable == null) {
        preTable = sortedTable;
        if (userContext.getBool(MULTI_TABLE, false)) {
          sb.append(StrUtil.format("{} AS {}", sortedTable, tableAlias(sortedTable)));
        } else {
          sb.append(StrUtil.format("{}", sortedTable));
        }
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

  private PropertyDescriptor findProperty(String propertyName) {
    PropertyDescriptor propertyDescriptor =
        CollectionUtil.findOne(this.allProperties, p -> p.getName().equals(propertyName));
    if (propertyDescriptor != null) {
      return propertyDescriptor;
    }
    throw new RepositoryException("Property: " + propertyName + " not defined");
  }

  private String prepareLimit(SearchRequest request) {
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
      return ((SQLRepository) repository).jdbcTemplate.getJdbcTemplate().getDataSource()
          == this.jdbcTemplate.getJdbcTemplate().getDataSource();
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
      SQLColumn childTypeCell = new SQLColumn(thisPrimaryTableName, CHILD_TYPE);
      childTypeCell.setType(CHILD_SQL_TYPE);
      allColumns.add(childTypeCell);
    }
    Map<String, List<SQLColumn>> tableColumns =
        CollStreamUtil.groupByKey(allColumns, SQLColumn::getTableName);
    tableColumns.forEach(
        (table, columns) -> {
          String sql =
              String.format(
                  "select * from information_schema.columns where table_name = '%s'", table);
          List<Map<String, Object>> dbTableInfo;
          try {
            dbTableInfo = jdbcTemplate.getJdbcTemplate().queryForList(sql);
          } catch (Exception exception) {
            dbTableInfo = ListUtil.empty();
          }
          ensure(ctx, dbTableInfo, table, columns);
        });
    ensureInitData(ctx);
    ensureIndexAndForeignKey(ctx);
    ensureIdSpaceTable(ctx);
  }

  private void ensureIdSpaceTable(UserContext ctx) {
    String sql =
        String.format(
            "select * from information_schema.columns where table_name = '%s'",
            TEAQL_ID_SPACE_TABLE);
    List<Map<String, Object>> dbTableInfo;
    try {
      dbTableInfo = jdbcTemplate.getJdbcTemplate().queryForList(sql);
    } catch (Exception exception) {
      dbTableInfo = ListUtil.empty();
    }

    if (!ObjectUtil.isEmpty(dbTableInfo)) {
      return;
    }

    StringBuilder sb = new StringBuilder();
    sb.append("CREATE TABLE ")
        .append(TEAQL_ID_SPACE_TABLE)
        .append(" (\n")
        .append("type_name varchar(100) PRIMARY KEY,\n")
        .append("current_level bigint);\n");
    String createIdSpaceSql = sb.toString();
    ctx.info(createIdSpaceSql);
    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      jdbcTemplate.getJdbcTemplate().execute(createIdSpaceSql);
    }
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

    TransactionTemplate transactionTemplate = userContext.getBean(TransactionTemplate.class);
    return transactionTemplate.execute(
        status -> {
          String type = CollectionUtil.getLast(types);
          Long current =
              jdbcTemplate
                  .getJdbcTemplate()
                  .queryForObject(
                      StrUtil.format(
                          "SELECT current_level from {} WHERE type_name = '{}' for update",
                          TEAQL_ID_SPACE_TABLE,
                          type),
                      Long.class);
          if (current == null) {
            current = 1l;
            jdbcTemplate
                .getJdbcTemplate()
                .execute(
                    StrUtil.format(
                        "INSERT INTO {} VALUES ('{}', {})", TEAQL_ID_SPACE_TABLE, type, current));
            return current;
          }
          current += 1;
          jdbcTemplate
              .getJdbcTemplate()
              .execute(
                  StrUtil.format(
                      "UPDATE {} SET current_level = {} WHERE type_name = '{}'",
                      TEAQL_ID_SPACE_TABLE,
                      current,
                      type));
          return current;
        });
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
            jdbcTemplate
                .getJdbcTemplate()
                .queryForMap(
                    StrUtil.format(
                        "SELECT * FROM {} WHERE id = {}",
                        tableName(entityDescriptor.getType()),
                        genIdForCandidateCode(code)));
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
                "UPDATE {} SET version = {} where id = '{}';\n",
                tableName(entityDescriptor.getType()),
                -version,
                genIdForCandidateCode(code));
        ctx.info(sql);
        if (ctx.config() != null && ctx.config().isEnsureTable()) {
          jdbcTemplate.getJdbcTemplate().execute(sql);
        }
        return;
      }

      String sql =
          StrUtil.format(
              "INSERT INTO {} ({}) VALUES ({});\n",
              tableName(entityDescriptor.getType()),
              CollectionUtil.join(columns, ","),
              CollectionUtil.join(
                  oneConstant, ",", a -> StrUtil.wrapIfMissing(String.valueOf(a), "'", "'")));
      ctx.info(sql);
      if (ctx.config() != null && ctx.config().isEnsureTable()) {
        jdbcTemplate.getJdbcTemplate().execute(sql);
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

    String autoFunction = property.getAdditionalInfo().get("autoFunction");
    if (!ObjectUtil.isEmpty(autoFunction)) {
      return ReflectUtil.invoke(ctx, autoFunction);
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
          jdbcTemplate
              .getJdbcTemplate()
              .queryForMap(
                  StrUtil.format(
                      "SELECT * FROM {} WHERE id = 1", tableName(entityDescriptor.getType())));
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
              "UPDATE {} SET version = {} where id = '1';\n",
              tableName(entityDescriptor.getType()),
              -version);
      ctx.info(sql);
      if (ctx.config() != null && ctx.config().isEnsureTable()) {
        jdbcTemplate.getJdbcTemplate().execute(sql);
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
            "INSERT INTO {} ({}) VALUES ({});\n",
            tableName(entityDescriptor.getType()),
            CollectionUtil.join(columns, ","),
            CollectionUtil.join(
                rootRow, ",", a -> StrUtil.wrapIfMissing(String.valueOf(a), "'", "'")));
    ctx.info(sql);
    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      jdbcTemplate.getJdbcTemplate().execute(sql);
    }
  }

  private Object getRootPropertyValue(UserContext ctx, PropertyDescriptor property) {
    if (property.isId()) {
      return 1l;
    }
    if (property.isVersion()) {
      return 1l;
    }
    String autoFunction = property.getAdditionalInfo().get("autoFunction");
    if (!ObjectUtil.isEmpty(autoFunction)) {
      return ReflectUtil.invoke(ctx, autoFunction);
    }
    return property.getAdditionalInfo().get("candidates");
  }

  private void ensure(
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
      String dbColumnName = StrUtil.unWrap(columnName, '\"');
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

  private String calculateDBType(Map<String, Object> columnInfo) {
    String dataType = (String) columnInfo.get("data_type");
    switch (dataType) {
      case "bigint":
        return "bigint";
      case "boolean":
        return "boolean";
      case "character varying":
        return StrUtil.format("varchar({})", columnInfo.get("character_maximum_length"));
      case "date":
        return "date";
      case "integer":
        return "integer";
      case "numeric":
        return StrUtil.format(
            "numeric({},{})", columnInfo.get("numeric_precision"), columnInfo.get("numeric_scale"));
      case "text":
        return "text";
      case "time without time zone":
        return "time";
      case "timestamp without time zone":
        return "timestamp";
      default:
        throw new RepositoryException("未处理的类型:" + dataType);
    }
  }

  private void alterColumn(UserContext ctx, String tableName, String columnName, String type) {
    String alterColumnSql =
        StrUtil.format("ALTER TABLE {} ALTER COLUMN {} TYPE {};", tableName, columnName, type);
    ctx.info(alterColumnSql);
    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      jdbcTemplate.getJdbcTemplate().execute(alterColumnSql);
    }
  }

  private void addColumn(
      UserContext ctx, String tableName, String preColumnName, String columnName, String type) {
    String addColumnSql =
        StrUtil.format("ALTER TABLE {} ADD COLUMN {} {};", tableName, columnName, type);
    ctx.info(addColumnSql);
    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      jdbcTemplate.getJdbcTemplate().execute(addColumnSql);
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
    sb.append(");\n");
    String createTableSql = sb.toString();
    ctx.info(createTableSql);

    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      jdbcTemplate.getJdbcTemplate().execute(createTableSql);
    }
  }

  @Override
  public List<SQLColumn> getPropertyColumns(String idTable, String propertyName) {
    if (CHILD_TYPE.equalsIgnoreCase(propertyName)) {
      if (entityDescriptor.hasChildren()) {
        SQLColumn sqlColumn = new SQLColumn(tableAlias(thisPrimaryTableName), CHILD_TYPE);
        sqlColumn.setType(CHILD_SQL_TYPE);
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
}
