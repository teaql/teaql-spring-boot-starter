package io.teaql.data.snowflake;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import cn.hutool.core.util.StrUtil;

import io.teaql.data.Entity;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLRepository;

public class SnowflakeRepository<T extends Entity> extends SQLRepository<T> {
    public SnowflakeRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
        super(entityDescriptor, dataSource);
    }

    @Override
    protected void ensureIndexAndForeignKey(UserContext ctx) {
    }

    @Override
    protected String findTableColumnsSql(DataSource dataSource, String table) {
        try (Connection connection = dataSource.getConnection()) {
            String databaseName = connection.getCatalog();
            String schemaName = connection.getSchema();
            return String.format(
                    "select * from information_schema.columns where table_name = '%s' and table_schema = '%s' and table_catalog = '%s'",
                    table.toUpperCase(), schemaName, databaseName);
        }
        catch (SQLException pE) {
            throw new RuntimeException(pE);
        }
    }

    @Override
    protected String getPureColumnName(String columnName) {
        return StrUtil.unWrap(columnName.toUpperCase(), '\"');
    }

    @Override
    protected String getSQLForUpdateWhenPrepareId() {

        return "SELECT current_level from {} WHERE type_name = '{}'";
    }

    @Override
    protected String calculateDBType(Map<String, Object> columnInfo) {
        String dataType = ((String) columnInfo.get("data_type")).toLowerCase();
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
            case "number":
                if (!columnInfo.get("numeric_scale").equals("0")) {
                    return StrUtil.format(
                            "numeric({},{})",
                            columnInfo.get("numeric_precision"),
                            columnInfo.get("numeric_scale"));
                }
                return "number";
            case "decimal":
            case "numeric":
                return StrUtil.format(
                        "numeric({},{})", columnInfo.get("numeric_precision"), columnInfo.get("numeric_scale"));
            case "text":
                if ("100".equals(columnInfo.get("character_maximum_length"))) {
                    return StrUtil.format("varchar({})", columnInfo.get("character_maximum_length"));
                }
                return "text";
            case "time without time zone":
                return "time";
            case "timestamp":
            case "timestamp_ntz":
            case "timestamp without time zone":
                return "timestamp";
            default:
                throw new RepositoryException("unsupported type:" + dataType);
        }
    }

    @Override
    protected void ensure(
            UserContext ctx, List<Map<String, Object>> tableInfo, String table, List<SQLColumn> columns) {
        List<Map<String, Object>> upperCaseTableInfo = new ArrayList<>();
        for (Map<String, Object> column : tableInfo) {
            Map<String, Object> upperCase = new HashMap<>();
            for (Map.Entry<String, Object> field : column.entrySet()) {
                if (field.getValue() != null) {
                    upperCase.put(field.getKey().toLowerCase(), field.getValue().toString().toUpperCase());
                }
            }
            upperCaseTableInfo.add(upperCase);
        }
        super.ensure(ctx, upperCaseTableInfo, table, columns);
    }
}
