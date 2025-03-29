package io.teaql.data.db2;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.util.StrUtil;

import io.teaql.data.BaseEntity;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLRepository;

public class DB2Repository<T extends BaseEntity> extends SQLRepository<T> {
    public DB2Repository(EntityDescriptor entityDescriptor, DataSource dataSource) {
        super(entityDescriptor, dataSource);
    }

    protected String wrapColumnStatementForCreatingTable(UserContext ctx, String table, SQLColumn column) {

        String dbColumn = column.getColumnName() + " " + column.getType();
        if (column.isIdColumn()) {
            dbColumn = dbColumn + " PRIMARY KEY NOT NULL";
        }
        return dbColumn;
    }

    public String getIdSpaceSql() {
        StringBuilder sb = new StringBuilder();
        sb.append("CREATE TABLE ")
                .append(getTqlIdSpaceTable())
                .append(" (\n")
                .append("type_name varchar(100) PRIMARY KEY NOT NULL,\n")
                .append("current_level bigint)\n");
        String createIdSpaceSql = sb.toString();
        return createIdSpaceSql;
    }

    @Override
    protected String findTableColumnsSql(DataSource dataSource, String table) {
        try (Connection connection = dataSource.getConnection()) {
            String databaseName = connection.getCatalog();
            String schemaName = connection.getSchema();
            return String.format(
                    "select * from sysibm.syscolumns WHERE tbcreator = '%s' AND tbname = '%s'",
                    schemaName, table.toUpperCase());
        }
        catch (SQLException pE) {
            throw new RuntimeException(pE);
        }
    }

    protected String getSchemaColumnNameFieldName() {
        return "name";
    }

    protected String getPureColumnName(String columnName) {
        return StrUtil.unWrap(columnName, '\"').toUpperCase();
    }

    protected String calculateDBType(Map<String, Object> columnInfo) {
        String dataType = ((String) columnInfo.get("coltype")).toLowerCase().trim();
        switch (dataType) {
            case "bigint":
                return "bigint";
            case "tinyint":
            case "boolean":
                return "boolean";
            case "varchar":
            case "character varying":
                return StrUtil.format("varchar({})", columnInfo.get("length"));
            case "date":
                return "date";
            case "int":
            case "integer":
                return "integer";
            case "decimal":
            case "numeric":
                return StrUtil.format(
                        "decimal({},{})", columnInfo.get("length"), columnInfo.get("scale"));
            case "text":
                return "text";
            case "clob":
                return "clob";
            case "time without time zone":
                return "time";
            case "timestamp":
            case "timestmp":
            case "timestamp without time zone":
                return "timestamp";
            default:
                throw new RepositoryException("unsupported type:" + dataType);
        }
    }

    protected Map<String, Map<String, Object>> getFields(List<Map<String, Object>> tableInfo) {
        Map<String, Map<String, Object>> result = CollStreamUtil.toIdentityMap(tableInfo, m -> String.valueOf(m.get(getSchemaColumnNameFieldName())));
        List<String> keys = new ArrayList<>(result.keySet());
        for (String key : keys) {
            if (key.equals(key.toUpperCase())) {
                continue;
            }
            result.put(key.toUpperCase(), result.get(key));
        }
        return result;
    }

    @Override
    protected void ensureIndexAndForeignKey(UserContext ctx) {
    }
}
