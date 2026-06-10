package io.teaql.data.android;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.teaql.data.AggregationItem;
import io.teaql.data.AggregationResult;
import io.teaql.data.Aggregations;
import io.teaql.data.BaseEntity;
import io.teaql.data.ConcurrentModifyException;
import io.teaql.data.Entity;
import io.teaql.data.Expression;
import io.teaql.data.OrderBy;
import io.teaql.data.OrderBys;
import io.teaql.data.Repository;
import io.teaql.data.RepositoryException;
import io.teaql.data.SearchCriteria;
import io.teaql.data.SearchRequest;
import io.teaql.data.SimpleNamedExpression;
import io.teaql.data.Slice;
import io.teaql.data.SmartList;
import io.teaql.data.UserContext;
import io.teaql.data.log.Markers;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.PropertyType;
import io.teaql.data.meta.Relation;
import io.teaql.data.repository.AbstractRepository;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLColumnResolver;
import io.teaql.data.sql.SQLConstraint;
import io.teaql.data.sql.SQLData;
import io.teaql.data.sql.SQLEntity;
import io.teaql.data.sql.SQLLogger;
import io.teaql.data.sql.SQLProperty;
import io.teaql.data.sql.SQLRepository;
import io.teaql.data.sql.expression.ExpressionHelper;
import io.teaql.data.sql.expression.SQLExpressionParser;
import io.teaql.data.utils.CollStreamUtil;
import io.teaql.data.utils.CollectionUtil;
import io.teaql.data.utils.ListUtil;
import io.teaql.data.utils.MapUtil;
import io.teaql.data.utils.NamingCase;
import io.teaql.data.utils.NumberUtil;
import io.teaql.data.utils.ObjectUtil;
import io.teaql.data.utils.ReflectUtil;
import io.teaql.data.utils.StrUtil;

/**
 * Android 专用 Repository 实现。
 * 不依赖 spring-jdbc，通过 TeaQLDatabase 抽象层访问 SQLite。
 * 复用 SQLRepository 的 SQL 构建逻辑（buildDataSQL 等）。
 */
public class AndroidRepository<T extends Entity> extends AbstractRepository<T>
        implements SQLColumnResolver {

    private static final Pattern NAMED_PARAM = Pattern.compile(":(\\w+)");

    public static final String TYPE_ALIAS = "_type_";
    public static final String IGNORE_SUBTYPES = "IGNORE_SUBTYPES";
    public static final String MULTI_TABLE = "MULTI_TABLE";

    private final EntityDescriptor entityDescriptor;
    private final TeaQLDatabase database;
    private String childType = "_child_type";
    private String childSqlType = "VARCHAR(100)";
    private String tqlIdSpaceTable = "teaql_id_space";
    private String versionTableName;
    private List<String> primaryTableNames = new ArrayList<>();
    private String thisPrimaryTableName;
    private Set<String> allTableNames = new LinkedHashSet<>();
    private List<String> types = new ArrayList<>();
    private List<String> auxiliaryTableNames;
    private List<PropertyDescriptor> allProperties = new ArrayList<>();
    private Map<Class, SQLExpressionParser> expressionParsers = new ConcurrentHashMap<>();

    public AndroidRepository(EntityDescriptor entityDescriptor, TeaQLDatabase database) {
        this.entityDescriptor = entityDescriptor;
        this.database = database;
        initSQLMeta(entityDescriptor);
    }

    // ==========================================
    // SQL 构建逻辑 (复用 SQLRepository)
    // ==========================================

    public String buildDataSQL(UserContext userContext, SearchRequest request, Map<String, Object> parameters) {
        String rawSql = request.getRawSql();
        if (ObjectUtil.isNotEmpty(rawSql)) {
            return rawSql;
        }

        List<String> tables = collectDataTables(userContext, request);
        String idTable = tables.get(0);
        Object preConfig = userContext.getObj(MULTI_TABLE);
        userContext.put(MULTI_TABLE, tables.size() > 1);
        try {
            String whereSql = prepareCondition(userContext, idTable, request.getSearchCriteria(), parameters);
            if (SearchCriteria.FALSE.equalsIgnoreCase(whereSql)) {
                return null;
            }
            if (request.getSlice() != null && request.getSlice().getSize() == 0) {
                return null;
            }

            String tableSQl = joinTables(userContext, tables);
            String selectSql = collectSelectSql(userContext, request, idTable, parameters);

            String partitionProperty = request.getPartitionProperty();
            if (ObjectUtil.isNotEmpty(partitionProperty) && request.getSlice() != null) {
                ensureOrderByForPartition(request);
            }

            String orderBySql = prepareOrderBy(userContext, request, idTable, parameters);

            if (!ObjectUtil.isEmpty(partitionProperty) && request.getSlice() != null) {
                PropertyDescriptor partitionPropertyDescriptor = findProperty(partitionProperty);
                SQLColumn sqlColumn = getSqlColumn(partitionPropertyDescriptor);
                String partitionTable = partitionPropertyDescriptor.isId() ? idTable : sqlColumn.getTableName();

                if (whereSql != null) whereSql = "WHERE " + whereSql;
                return StrUtil.format(
                        "SELECT * FROM (SELECT {}, (row_number() over(partition by {}{} {})) as _rank from {} {}) as t where t._rank >= {} and t._rank < {}",
                        selectSql,
                        userContext.getBool(MULTI_TABLE, false) ? tableAlias(partitionTable) + "." : "",
                        sqlColumn.getColumnName(), orderBySql, tableSQl, whereSql,
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
        } finally {
            userContext.put(MULTI_TABLE, preConfig);
        }
    }

    // ==========================================
    // 命名参数 → 位置参数转换
    // ==========================================

    private static class PositionalSQL {
        final String sql;
        final Object[] args;

        PositionalSQL(String sql, Object[] args) {
            this.sql = sql;
            this.args = args;
        }
    }

    private PositionalSQL toPositional(String namedSql, Map<String, Object> params) {
        List<Object> args = new ArrayList<>();
        Matcher m = NAMED_PARAM.matcher(namedSql);
        StringBuffer sb = new StringBuffer();
        while (m.find()) {
            String paramName = m.group(1);
            Object value = params.get(paramName);
            args.add(value);
            m.appendReplacement(sb, "?");
        }
        m.appendTail(sb);
        return new PositionalSQL(sb.toString(), args.toArray());
    }

    // ==========================================
    // 数据操作 (TeaQLDatabase 替代 spring-jdbc)
    // ==========================================

    @Override
    public EntityDescriptor getEntityDescriptor() {
        return this.entityDescriptor;
    }

    public SmartList<T> loadInternal(UserContext userContext, SearchRequest<T> request) {
        Map<String, Object> params = new HashMap<>();
        String sql = buildDataSQL(userContext, request, params);
        if (ObjectUtil.isEmpty(sql)) {
            return new SmartList<>();
        }
        PositionalSQL psql = toPositional(sql, params);
        List<Map<String, Object>> rows = database.query(psql.sql, psql.args);
        List<T> results = rows.stream()
                .map(row -> mapRowToEntity(userContext, request, row))
                .collect(Collectors.toList());
        return new SmartList<>(results);
    }

    private T mapRowToEntity(UserContext userContext, SearchRequest<T> request, Map<String, Object> row) {
        Class<? extends T> returnType = request.returnType();
        T entity = ReflectUtil.newInstance(returnType);
        for (PropertyDescriptor property : this.allProperties) {
            if (!shouldHandle(property)) continue;
            if (property instanceof SQLProperty) {
                Object value = row.get(property.getName());
                if (value != null) {
                    Class targetType = property.getType().javaType();
                    entity.setProperty(property.getName(),
                            io.teaql.data.utils.Convert.convert(targetType, value));
                }
            }
        }
        // 子类型
        Object typeAlias = row.get(TYPE_ALIAS);
        if (typeAlias != null) {
            entity.setRuntimeType(String.valueOf(typeAlias));
        }
        // 状态
        Long version = entity.getVersion();
        if (version != null && version < 0) {
            if (entity instanceof BaseEntity) ((BaseEntity) entity).set$status(io.teaql.data.EntityStatus.PERSISTED_DELETED);
        } else {
            if (entity instanceof BaseEntity) ((BaseEntity) entity).set$status(io.teaql.data.EntityStatus.PERSISTED);
        }
        // 动态属性
        List<SimpleNamedExpression> simpleDynamicProperties = request.getSimpleDynamicProperties();
        for (SimpleNamedExpression dp : simpleDynamicProperties) {
            Object value = row.get(dp.name());
            if (value != null) entity.addDynamicProperty(dp.name(), value);
        }
        userContext.afterLoad(getEntityDescriptor(), entity);
        return entity;
    }

    @Override
    public void createInternal(UserContext userContext, Collection<T> createItems) {
        List<SQLEntity> sqlEntities = CollectionUtil.map(createItems,
                i -> convertToSQLEntityForInsert(userContext, i), true);
        if (ObjectUtil.isEmpty(sqlEntities)) return;

        SQLEntity sqlEntity = sqlEntities.get(0);
        Map<String, List<String>> tableColumns = sqlEntity.getTableColumnNames();

        Map<String, List<Object[]>> rows = new HashMap<>();
        for (SQLEntity entity : sqlEntities) {
            Map<String, List> tableColumnValues = entity.getTableColumnValues();
            for (Map.Entry<String, List> entry : tableColumnValues.entrySet()) {
                String k = entry.getKey();
                List v = entry.getValue();
                List<Object[]> values = rows.computeIfAbsent(k, key -> new ArrayList<>());
                if (auxiliaryTableNames.contains(k) && entity.allNullExceptID(v)) continue;
                values.add(v.toArray());
            }
        }

        TreeMap<String, List<Object[]>> sorted = MapUtil.sort(rows, (t1, t2) -> {
            if (t1.equals(versionTableName)) return -1;
            if (t2.equals(versionTableName)) return 1;
            return 0;
        });

        sorted.forEach((k, v) -> {
            if (v.isEmpty()) return;
            List<String> columns = tableColumns.get(k);
            String sql = StrUtil.format("INSERT INTO {} ({}) VALUES ({})",
                    k, CollectionUtil.join(columns, ","),
                    StrUtil.repeatAndJoin("?", columns.size(), ","));
            database.batchUpdate(sql, v);
        });
    }

    @Override
    public void updateInternal(UserContext userContext, Collection<T> updateItems) {
        if (ObjectUtil.isEmpty(updateItems)) return;
        List<SQLEntity> sqlEntities = CollectionUtil.map(updateItems,
                i -> convertToSQLEntityForUpdate(userContext, i), true);
        if (ObjectUtil.isEmpty(sqlEntities)) return;

        for (SQLEntity sqlEntity : sqlEntities) {
            if (sqlEntity.isEmpty()) continue;
            Map<String, List<String>> tableColumnNames = sqlEntity.getTableColumnNames();
            Map<String, List> tableColumnValues = sqlEntity.getTableColumnValues();

            AtomicBoolean versionTableUpdated = new AtomicBoolean(false);
            tableColumnValues.forEach((k, v) -> {
                List<String> columns = new ArrayList<>(tableColumnNames.get(k));
                List l = new ArrayList(v);
                boolean versionTable = this.versionTableName.equals(k);
                boolean primaryTable = this.primaryTableNames.contains(k);

                if (versionTable) {
                    updateVersionTable(userContext, sqlEntity, versionTableUpdated, k, columns, l);
                } else if (primaryTable) {
                    updatePrimaryTable(userContext, sqlEntity, k, columns, l);
                } else {
                    String updateSql = StrUtil.format("REPLACE INTO {} SET {}",
                            k, columns.stream().map(c -> c + " = ?").collect(Collectors.joining(" , ")));
                    database.executeUpdate(updateSql, l.toArray());
                }
            });

            if (!versionTableUpdated.get()) {
                updateVersionTableVersion(userContext, sqlEntity);
            }
        }
    }

    private void updateVersionTableVersion(UserContext userContext, SQLEntity sqlEntity) {
        String updateSql = StrUtil.format("UPDATE {} SET {} = ? WHERE {} = ? and {} = ?",
                this.versionTableName, "version", "id", "version");
        Object[] parameters = {sqlEntity.getVersion() + 1, sqlEntity.getId(), sqlEntity.getVersion()};
        int update = database.executeUpdate(updateSql, parameters);
        if (update != 1) throw new ConcurrentModifyException();
    }

    private void updatePrimaryTable(UserContext userContext, SQLEntity sqlEntity, String k, List<String> columns, List l) {
        l.add(sqlEntity.getId());
        String updateSql = StrUtil.format("UPDATE {} SET {} WHERE id = ?",
                k, columns.stream().map(c -> c + " = ?").collect(Collectors.joining(" , ")));
        int update = database.executeUpdate(updateSql, l.toArray());
        if (update != 1) throw new RepositoryException("primary table update failed");
    }

    private void updateVersionTable(UserContext userContext, SQLEntity sqlEntity,
                                     AtomicBoolean versionTableUpdated, String k, List<String> columns, List l) {
        versionTableUpdated.set(true);
        columns.add("version");
        l.add(sqlEntity.getVersion() + 1);
        l.add(sqlEntity.getId());
        l.add(sqlEntity.getVersion());
        String updateSql = StrUtil.format("UPDATE {} SET {} WHERE id = ? AND version = ?",
                k, columns.stream().map(c -> c + " = ?").collect(Collectors.joining(" , ")));
        int update = database.executeUpdate(updateSql, l.toArray());
        if (update != 1) throw new ConcurrentModifyException();
    }

    @Override
    public void deleteInternal(UserContext userContext, Collection<T> entities) {
        if (ObjectUtil.isEmpty(entities)) return;
        String updateSql = StrUtil.format("UPDATE {} SET version = ? WHERE id = ? AND version = ?", this.versionTableName);
        List<Object[]> args = entities.stream()
                .filter(e -> e.getVersion() > 0)
                .map(e -> new Object[]{-(e.getVersion() + 1), e.getId(), e.getVersion()})
                .collect(Collectors.toList());
        int[] rets = database.batchUpdate(updateSql, args);
        for (int ret : rets) {
            if (ret != 1) throw new ConcurrentModifyException();
        }
    }

    @Override
    public void recoverInternal(UserContext userContext, Collection<T> entities) {
        if (ObjectUtil.isEmpty(entities)) return;
        String updateSql = StrUtil.format("UPDATE {} SET version = ? WHERE id = ? AND version = ?", this.versionTableName);
        List<Object[]> args = entities.stream()
                .filter(e -> e.getVersion() < 0)
                .map(e -> new Object[]{(-e.getVersion() + 1), e.getId(), e.getVersion()})
                .collect(Collectors.toList());
        int[] rets = database.batchUpdate(updateSql, args);
        for (int ret : rets) {
            if (ret != 1) throw new ConcurrentModifyException();
        }
    }

    // ==========================================
    // ID 生成
    // ==========================================

    @Override
    public Long prepareId(UserContext userContext, T entity) {
        if (entity.getId() != null) return entity.getId();
        Long id = userContext.generateId(entity);
        if (id != null) return id;

        String type = CollectionUtil.getLast(types);
        AtomicLong current = new AtomicLong();

        database.executeInTransaction(() -> {
            Number dbCurrent = null;
            try {
                List<Map<String, Object>> rows = database.query(
                        StrUtil.format("SELECT current_level from {} WHERE type_name = '{}'", getTqlIdSpaceTable(), type),
                        new Object[0]);
                if (!rows.isEmpty()) {
                    Object val = rows.get(0).get("current_level");
                    if (val instanceof Number) dbCurrent = (Number) val;
                    else if (val != null) dbCurrent = Long.parseLong(String.valueOf(val));
                }
            } catch (Exception ignored) {
            }

            if (dbCurrent == null) {
                current.set(1L);
                database.executeUpdate(
                        StrUtil.format("INSERT INTO {} VALUES ('{}', {})", getTqlIdSpaceTable(), type, current),
                        new Object[0]);
            } else {
                dbCurrent = NumberUtil.add(dbCurrent, 1);
                database.executeUpdate(
                        StrUtil.format("UPDATE {} SET current_level = {} WHERE type_name = '{}'",
                                getTqlIdSpaceTable(), dbCurrent, type),
                        new Object[0]);
                current.set(dbCurrent.longValue());
            }
        });
        return current.get();
    }

    // ==========================================
    // Schema 管理
    // ==========================================

    public void ensureSchema(UserContext ctx) {
        List<SQLColumn> allColumns = new ArrayList<>();
        for (PropertyDescriptor ownProperty : entityDescriptor.getOwnProperties()) {
            allColumns.addAll(getSqlColumns(ownProperty));
        }
        if (entityDescriptor.hasChildren()) {
            SQLColumn childTypeCell = new SQLColumn(thisPrimaryTableName, getChildType());
            childTypeCell.setType(getChildSqlType());
            allColumns.add(childTypeCell);
        }

        Map<String, List<SQLColumn>> tableColumns = CollStreamUtil.groupByKey(allColumns, SQLColumn::getTableName);
        tableColumns.forEach((table, columns) -> {
            List<Map<String, Object>> dbTableInfo;
            try {
                dbTableInfo = database.getTableColumns(table);
            } catch (Exception e) {
                dbTableInfo = ListUtil.empty();
            }
            ensure(ctx, dbTableInfo, table, columns);
        });

        ensureInitData(ctx);
        ensureIdSpaceTable(ctx);
    }

    public void ensureIdSpaceTable(UserContext ctx) {
        List<Map<String, Object>> dbTableInfo;
        try {
            dbTableInfo = database.getTableColumns(getTqlIdSpaceTable());
        } catch (Exception e) {
            dbTableInfo = ListUtil.empty();
        }
        if (!ObjectUtil.isEmpty(dbTableInfo)) return;

        String sql = "CREATE TABLE " + getTqlIdSpaceTable() + " (\n"
                + "type_name varchar(100) PRIMARY KEY,\n"
                + "current_level bigint)\n";
        ctx.info(sql + ";");
        if (ctx.config() != null && ctx.config().isEnsureTable()) {
            try { database.execute(sql); } catch (Exception e) { ctx.info("Ignored: " + e.getMessage()); }
        }
    }

    protected void ensure(UserContext ctx, List<Map<String, Object>> tableInfo, String table, List<SQLColumn> columns) {
        if (tableInfo.isEmpty()) {
            createTable(ctx, table, columns);
            return;
        }
        Map<String, Map<String, Object>> fields = CollStreamUtil.toIdentityMap(
                tableInfo, m -> String.valueOf(m.get("column_name")).toLowerCase());
        for (SQLColumn column : columns) {
            String dbColumnName = column.getColumnName().toLowerCase();
            if (!fields.containsKey(dbColumnName)) {
                addColumn(ctx, column);
            }
        }
    }

    protected void createTable(UserContext ctx, String table, List<SQLColumn> columns) {
        StringBuilder sb = new StringBuilder();
        sb.append("CREATE TABLE ").append(table).append(" (\n");
        sb.append(columns.stream()
                .map(column -> {
                    String dbColumn = column.getColumnName() + " " + column.getType();
                    if (column.isIdColumn()) dbColumn += " PRIMARY KEY";
                    return dbColumn;
                })
                .collect(Collectors.joining(",\n")));
        sb.append(")\n");
        ctx.info(sb + ";");
        if (ctx.config() != null && ctx.config().isEnsureTable()) {
            try { database.execute(sb.toString()); } catch (Exception e) { ctx.info("Ignored: " + e.getMessage()); }
        }
    }

    protected void addColumn(UserContext ctx, SQLColumn column) {
        String sql = StrUtil.format("ALTER TABLE {} ADD COLUMN {} {}",
                column.getTableName(), column.getColumnName(), column.getType());
        ctx.info(sql + ";");
        if (ctx.config() != null && ctx.config().isEnsureTable()) {
            try { database.execute(sql); } catch (Exception e) { ctx.info("Ignored: " + e.getMessage()); }
        }
    }

    public void ensureInitData(UserContext ctx) {
        if (entityDescriptor.isRoot()) ensureRoot(ctx);
        if (entityDescriptor.isConstant()) ensureConstant(ctx);
    }

    private void ensureRoot(UserContext ctx) {
        List<Map<String, Object>> dbRow;
        try {
            dbRow = database.query(
                    StrUtil.format("SELECT * FROM {} WHERE id = '1'", tableName(entityDescriptor.getType())),
                    new Object[0]);
        } catch (Exception e) {
            dbRow = ListUtil.empty();
        }

        if (!dbRow.isEmpty()) {
            long version = Long.parseLong(String.valueOf(dbRow.get(0).get("version")));
            if (version > 0) return;
            String sql = StrUtil.format("UPDATE {} SET version = {} where id = '1'", tableName(entityDescriptor.getType()), -version);
            ctx.info(sql + ";");
            if (ctx.config() != null && ctx.config().isEnsureTable()) {
                try { database.execute(sql); } catch (Exception e) { ctx.info("Ignored: " + e.getMessage()); }
            }
            return;
        }

        List<String> columns = new ArrayList<>();
        List<Object> rootRow = new ArrayList<>();
        for (PropertyDescriptor ownProperty : entityDescriptor.getOwnProperties()) {
            columns.add(getSqlColumn(ownProperty).getColumnName());
            rootRow.add(getRootPropertyValue(ctx, ownProperty));
        }
        String sql = StrUtil.format("INSERT INTO {} ({}) VALUES ({})",
                tableName(entityDescriptor.getType()),
                CollectionUtil.join(columns, ","),
                CollectionUtil.join(rootRow, ",", value -> getSqlValue(value)));
        ctx.info(sql + ";");
        if (ctx.config() != null && ctx.config().isEnsureTable()) {
            try { database.execute(sql); } catch (Exception e) { ctx.info("Ignored: " + e.getMessage()); }
        }
    }

    private void ensureConstant(UserContext ctx) {
        PropertyDescriptor identifier = entityDescriptor.getIdentifier();
        List<String> candidates = identifier.getCandidates();
        List<PropertyDescriptor> ownProperties = entityDescriptor.getOwnProperties();
        List<String> columns = ownProperties.stream()
                .map(p -> getSqlColumn(p).getColumnName())
                .collect(Collectors.toList());

        for (int idx = 0; idx < candidates.size(); idx++) {
            final int i = idx;
            String code = candidates.get(i);
            List<Object> oneConstant = ownProperties.stream()
                    .map(p -> getConstantPropertyValue(ctx, p, i, code))
                    .collect(Collectors.toList());

            try {
                List<Map<String, Object>> existing = database.query(
                        StrUtil.format("SELECT * FROM {} WHERE id = '{}'",
                                tableName(entityDescriptor.getType()),
                                getConstantPropertyValue(ctx, entityDescriptor.findIdProperty(), i, code)),
                        new Object[0]);
                if (!existing.isEmpty()) {
                    long version = Long.parseLong(String.valueOf(existing.get(0).get("version")));
                    if (version > 0) continue;
                    String sql = StrUtil.format("UPDATE {} SET version = {} where id = '{}'",
                            tableName(entityDescriptor.getType()), -version,
                            getConstantPropertyValue(ctx, entityDescriptor.findIdProperty(), i, code));
                    ctx.info(sql + ";");
                    if (ctx.config() != null && ctx.config().isEnsureTable()) {
                        try { database.execute(sql); } catch (Exception e) { ctx.info("Ignored: " + e.getMessage()); }
                    }
                    continue;
                }
            } catch (Exception ignored) {
            }

            String sql = StrUtil.format("INSERT INTO {} ({}) VALUES ({})",
                    tableName(entityDescriptor.getType()),
                    CollectionUtil.join(columns, ","),
                    CollectionUtil.join(oneConstant, ",", value -> getSqlValue(value)));
            ctx.info(sql + ";");
            if (ctx.config() != null && ctx.config().isEnsureTable()) {
                try { database.execute(sql); } catch (Exception e) { ctx.info("Ignored: " + e.getMessage()); }
            }
        }
    }

    // ==========================================
    // 辅助方法
    // ==========================================

    private SQLEntity convertToSQLEntityForInsert(UserContext userContext, T entity) {
        SQLEntity sqlEntity = new SQLEntity();
        sqlEntity.setId(entity.getId());
        sqlEntity.setVersion(entity.getVersion());
        for (PropertyDescriptor pd : this.allProperties) {
            if (pd instanceof Relation && !shouldHandle((Relation) pd)) continue;
            Object v = entity.getProperty(pd.getName());
            List<SQLData> data = convertToSQLData(userContext, entity, pd, v);
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

    private SQLEntity convertToSQLEntityForUpdate(UserContext userContext, T entity) {
        List<String> updatedProperties = entity.getUpdatedProperties();
        if (ObjectUtil.isEmpty(updatedProperties)) return null;
        SQLEntity sqlEntity = new SQLEntity();
        sqlEntity.setId(entity.getId());
        sqlEntity.setVersion(entity.getVersion());
        for (String updatedProperty : updatedProperties) {
            PropertyDescriptor property = findProperty(updatedProperty);
            if (property.isId() || property.isVersion()) continue;
            Object v = entity.getProperty(property.getName());
            List<SQLData> data = convertToSQLData(userContext, entity, property, v);
            sqlEntity.addPropertySQLData(data);
        }
        return sqlEntity;
    }

    private List<SQLData> convertToSQLData(UserContext ctx, T entity, PropertyDescriptor property, Object value) {
        if (property instanceof SQLProperty) {
            return ((SQLProperty) property).toDBRaw(ctx, entity, value);
        }
        throw new RepositoryException("AndroidRepository only supports SQLProperty");
    }

    private boolean shouldHandle(PropertyDescriptor pProperty) {
        if (pProperty instanceof Relation) return shouldHandle((Relation) pProperty);
        return true;
    }

    public boolean shouldHandle(Relation relation) {
        return relation.getRelationKeeper() == this.entityDescriptor;
    }

    private void initSQLMeta(EntityDescriptor entityDescriptor) {
        EntityDescriptor descriptor = entityDescriptor;
        while (descriptor != null) {
            types.add(descriptor.getType());
            for (PropertyDescriptor property : descriptor.getProperties()) {
                allProperties.add(property);
                if (property instanceof Relation && !shouldHandle((Relation) property)) continue;
                List<SQLColumn> sqlColumns = getSqlColumns(property);
                if (ObjectUtil.isEmpty(sqlColumns)) {
                    throw new RepositoryException("property :" + property.getName() + " miss sql table columns");
                }
                String firstTable = sqlColumns.get(0).getTableName();
                if (property.isVersion()) this.versionTableName = firstTable;
                if (property.isId()) {
                    if (!this.primaryTableNames.contains(firstTable)) this.primaryTableNames.add(firstTable);
                    if (property.getOwner() == this.entityDescriptor) this.thisPrimaryTableName = firstTable;
                }
                this.allTableNames.addAll(CollStreamUtil.toList(sqlColumns, SQLColumn::getTableName));
            }
            descriptor = descriptor.getParent();
        }
        this.auxiliaryTableNames = new ArrayList<>(CollectionUtil.subtract(this.allTableNames, this.primaryTableNames));
    }

    public PropertyDescriptor findProperty(String propertyName) {
        for (PropertyDescriptor pd : allProperties) {
            if (pd.getName().equals(propertyName)) return pd;
        }
        throw new RepositoryException("Property not found: " + propertyName);
    }

    private List<SQLColumn> getSqlColumns(PropertyDescriptor property) {
        if (property instanceof SQLProperty) return ((SQLProperty) property).columns();
        throw new RepositoryException("AndroidRepository only supports SQLProperty");
    }

    private SQLColumn getSqlColumn(PropertyDescriptor property) {
        return CollectionUtil.getFirst(getSqlColumns(property));
    }

    public String tableName(String type) {
        return NamingCase.toUnderlineCase(type + "_data");
    }

    private String tableAlias(String table) {
        return NamingCase.toCamelCase(table);
    }

    protected String getSqlValue(Object value) {
        if (value == null) return "NULL";
        if (value instanceof Number) return String.valueOf(value);
        if (value instanceof Boolean) return ((Boolean) value) ? "1" : "0";
        return StrUtil.wrapIfMissing(String.valueOf(value), "'", "'");
    }

    private Object getRootPropertyValue(UserContext ctx, PropertyDescriptor property) {
        if (property.isId()) return 1L;
        if (property.isVersion()) return 1L;
        String createFunction = property.getAdditionalInfo().get("createFunction");
        if (!ObjectUtil.isEmpty(createFunction)) return ReflectUtil.invoke(ctx, createFunction);
        return property.getAdditionalInfo().get("candidates");
    }

    private Object getConstantPropertyValue(UserContext ctx, PropertyDescriptor property, int index, String identifier) {
        if (property.isVersion()) return 1L;
        PropertyType type = property.getType();
        if (BaseEntity.class.isAssignableFrom(type.javaType())) return "1";
        String createFunction = property.getAdditionalInfo().get("createFunction");
        if (!ObjectUtil.isEmpty(createFunction)) return ReflectUtil.invoke(ctx, createFunction);
        List<String> candidates = property.getCandidates();
        if (property.isIdentifier()) return identifier;
        if (ObjectUtil.isNotEmpty(candidates)) return CollectionUtil.get(candidates, index);
        if (property.isId()) return Math.abs(identifier.toUpperCase().hashCode());
        return null;
    }

    private long genIdForCandidateCode(String code) {
        return Math.abs(code.toUpperCase().hashCode());
    }

    // ==========================================
    // SQL 构建辅助方法
    // ==========================================

    private String prepareCondition(UserContext userContext, String idTable, SearchCriteria searchCriteria, Map<String, Object> parameters) {
        if (ObjectUtil.isEmpty(searchCriteria)) return SearchCriteria.TRUE;
        return ExpressionHelper.toSql(userContext, searchCriteria, idTable, parameters, this);
    }

    private String prepareOrderBy(UserContext userContext, SearchRequest request, String idTable, Map<String, Object> parameters) {
        OrderBys orderBys = request.getOrderBy();
        if (ObjectUtil.isEmpty(orderBys)) return null;
        return ExpressionHelper.toSql(userContext, orderBys, idTable, parameters, this);
    }

    private String prepareLimit(SearchRequest request) {
        Slice slice = request.getSlice();
        if (ObjectUtil.isEmpty(slice)) return null;
        return StrUtil.format("LIMIT {} OFFSET {}", slice.getSize(), slice.getOffset());
    }

    private void ensureOrderByForPartition(SearchRequest<T> request) {
        OrderBys orderBy = request.getOrderBy();
        if (orderBy.isEmpty()) orderBy.addOrderBy(new OrderBy("id"));
    }

    public String joinTables(UserContext userContext, List<String> tables) {
        List<String> sortedTables = new ArrayList<>();
        for (String table : tables) if (primaryTableNames.contains(table)) sortedTables.add(table);
        for (String table : tables) if (!primaryTableNames.contains(table)) sortedTables.add(table);

        if (!userContext.getBool(MULTI_TABLE, false)) return sortedTables.get(0);

        StringBuilder sb = new StringBuilder();
        String preTable = null;
        for (String sortedTable : sortedTables) {
            if (preTable == null) {
                preTable = sortedTable;
                sb.append(StrUtil.format("{} AS {}", sortedTable, tableAlias(sortedTable)));
                continue;
            }
            sb.append(StrUtil.format(" {} JOIN {} AS {} ON {}.{} = {}.{}",
                    primaryTableNames.contains(sortedTable) ? "INNER" : "LEFT",
                    sortedTable, tableAlias(sortedTable),
                    tableAlias(sortedTable), "id", tableAlias(preTable), "id"));
        }
        return sb.toString();
    }

    private String collectSelectSql(UserContext userContext, SearchRequest request, String idTable, Map<String, Object> pParameters) {
        List<SimpleNamedExpression> allSelects = new ArrayList<>();
        List<SimpleNamedExpression> projections = request.getProjections();
        if (projections != null) allSelects.addAll(projections);
        List<SimpleNamedExpression> simpleDynamicProperties = request.getSimpleDynamicProperties();
        if (simpleDynamicProperties != null) allSelects.addAll(simpleDynamicProperties);

        String selects = allSelects.stream()
                .map(e -> ExpressionHelper.toSql(userContext, e, idTable, pParameters, this))
                .collect(Collectors.joining(", "));

        if (!userContext.getBool(IGNORE_SUBTYPES, false)) {
            String typeSQL = getTypeSQL(userContext);
            if (ObjectUtil.isNotEmpty(typeSQL)) selects = selects + ", " + typeSQL;
        }
        return selects;
    }

    private String getTypeSQL(UserContext userContext) {
        if (!getEntityDescriptor().hasChildren()) return null;
        if (userContext.getBool(MULTI_TABLE, false)) {
            return StrUtil.format("{}.{} AS {}", tableAlias(thisPrimaryTableName), getChildType(), TYPE_ALIAS);
        }
        return StrUtil.format("{} AS {}", getChildType(), TYPE_ALIAS);
    }

    private List<String> collectDataTables(UserContext userContext, SearchRequest<T> request) {
        return collectTablesFromProperties(userContext, request.dataProperties(userContext));
    }

    private ArrayList<String> collectTablesFromProperties(UserContext userContext, List<String> properties) {
        Set<String> tables = new HashSet<>();
        for (String target : properties) {
            PropertyDescriptor property = findProperty(target);
            if (property.isId()) continue;
            for (SQLColumn sqlColumn : getSqlColumns(property)) tables.add(sqlColumn.getTableName());
        }
        tables.add(thisPrimaryTableName);
        return ListUtil.toList(tables);
    }

    @Override
    public List<SQLColumn> getPropertyColumns(String idTable, String propertyName) {
        if (getChildType().equalsIgnoreCase(propertyName)) {
            if (entityDescriptor.hasChildren()) {
                SQLColumn sqlColumn = new SQLColumn(tableAlias(thisPrimaryTableName), getChildType());
                sqlColumn.setType(getChildSqlType());
                return ListUtil.of(sqlColumn);
            }
            return ListUtil.empty();
        }
        PropertyDescriptor property = findProperty(propertyName);
        List<SQLColumn> sqlColumns = getSqlColumns(property);
        for (SQLColumn sqlColumn : sqlColumns) {
            if (property.isId()) sqlColumn.setTableName(tableAlias(idTable));
            else sqlColumn.setTableName(tableAlias(sqlColumn.getTableName()));
        }
        return sqlColumns;
    }

    // ==========================================
    // 聚合查询
    // ==========================================

    @Override
    protected AggregationResult doAggregateInternal(UserContext userContext, SearchRequest<T> request) {
        if (!request.hasSimpleAgg()) return null;

        List<String> tables = collectAggregationTables(userContext, request);
        Map<String, Object> parameters = new HashMap<>();
        String idTable = tables.get(0);
        Object preConfig = userContext.getObj(MULTI_TABLE);
        userContext.put(MULTI_TABLE, tables.size() > 1);

        try {
            String whereSql = prepareCondition(userContext, idTable, request.getSearchCriteria(), parameters);
            if (SearchCriteria.FALSE.equalsIgnoreCase(whereSql)) return null;

            String selectSql = collectAggregationSelectSql(userContext, request, idTable, parameters);
            String sql = StrUtil.format("SELECT {} FROM {}", selectSql, joinTables(userContext, tables));
            if (whereSql != null && !SearchCriteria.TRUE.equalsIgnoreCase(whereSql)) {
                sql = StrUtil.format("{} WHERE {}", sql, whereSql);
            }

            String groupBy = collectAggregationGroupBySql(userContext, request, idTable, parameters);
            if (!ObjectUtil.isEmpty(groupBy)) sql = StrUtil.format("{} {}", sql, groupBy);

            PositionalSQL psql = toPositional(sql, parameters);
            List<Map<String, Object>> rows = database.query(psql.sql, psql.args);

            AggregationResult result = new AggregationResult();
            result.setName(request.getAggregations().getName());
            List<AggregationItem> items = rows.stream().map(row -> {
                AggregationItem item = new AggregationItem();
                for (SimpleNamedExpression function : request.getAggregations().getAggregates()) {
                    item.addValue(function, row.get(function.name()));
                }
                for (SimpleNamedExpression dimension : request.getAggregations().getDimensions()) {
                    item.addDimension(dimension, row.get(dimension.name()));
                }
                return item;
            }).collect(Collectors.toList());
            result.setData(items);
            return result;
        } finally {
            userContext.put(MULTI_TABLE, preConfig);
        }
    }

    private String collectAggregationGroupBySql(UserContext userContext, SearchRequest<T> request, String idTable, Map<String, Object> parameters) {
        List<SimpleNamedExpression> dimensions = request.getAggregations().getDimensions();
        if (dimensions.isEmpty()) return null;
        return dimensions.stream()
                .map(d -> { Expression e = d.getExpression(); while (e instanceof SimpleNamedExpression) e = ((SimpleNamedExpression)e).getExpression(); return e; })
                .map(e -> (String) ExpressionHelper.toSql(userContext, e, idTable, parameters, this))
                .collect(Collectors.joining(",", "GROUP BY ", ""));
    }

    private String collectAggregationSelectSql(UserContext userContext, SearchRequest<T> request, String idTable, Map<String, Object> params) {
        return request.getAggregations().getSelectedExpressions().stream()
                .map(e -> (String) ExpressionHelper.toSql(userContext, e, idTable, params, this))
                .collect(Collectors.joining(","));
    }

    private List<String> collectAggregationTables(UserContext userContext, SearchRequest<T> request) {
        return collectTablesFromProperties(userContext, request.aggregationProperties(userContext));
    }

    // ==========================================
    // Stream 支持
    // ==========================================

    @Override
    public Stream<T> executeForStream(UserContext userContext, SearchRequest<T> request, int enhanceBatch) {
        return loadInternal(userContext, request).stream();
    }

    // ==========================================
    // Getter/Setter
    // ==========================================

    public String getChildType() { return childType; }
    public void setChildType(String pChildType) { childType = pChildType; }
    public String getChildSqlType() { return childSqlType; }
    public void setChildSqlType(String pChildSqlType) { childSqlType = pChildSqlType; }
    public String getTqlIdSpaceTable() { return tqlIdSpaceTable; }
    public void setTqlIdSpaceTable(String pTqlIdSpaceTable) { tqlIdSpaceTable = pTqlIdSpaceTable; }
    public TeaQLDatabase getDatabase() { return database; }
}
