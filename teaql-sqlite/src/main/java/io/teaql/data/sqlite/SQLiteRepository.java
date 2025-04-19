package io.teaql.data.sqlite;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.sql.DataSource;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.map.CaseInsensitiveMap;
import cn.hutool.core.util.StrUtil;

import io.teaql.data.Entity;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLRepository;




public class SQLiteRepository<T extends Entity> extends SQLRepository<T> {
    public SQLiteRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
        super(entityDescriptor, dataSource);
        registerExpressionParser(SQLiteAggrExpressionParser.class);
        registerExpressionParser(SQLiteParameterParser.class);
        registerExpressionParser(SQLiteTwoOperatorExpressionParser.class);
    }
    //name

    protected String getSchemaColumnNameFieldName() {
        return "name";
    }

    public static class ParsedType {
        public String dataType;
        public Integer length; // 可以为 null

        @Override
        public String toString() {
            return "dataType='" + dataType + '\'' + ", length=" + length;
        }
    }

    public static ParsedType parseType(String type) {
        ParsedType result = new ParsedType();

        // 正则匹配：例如 VARCHAR(100)
        Pattern pattern = Pattern.compile("([a-zA-Z]+)\\s*\\(?\\s*(\\d*)\\s*\\)?");
        Matcher matcher = pattern.matcher(type.trim());

        if (matcher.find()) {
            result.dataType = matcher.group(1);
            String len = matcher.group(2);
            if (len != null && !len.isEmpty()) {
                result.length = Integer.parseInt(len);
            } else {
                result.length = null;
            }
        } else {
            result.dataType = type.trim();
            result.length = null;
        }

        return result;
    }


    protected String calculateDBType(Map<String, Object> columnInfo) {
        String dataType = (String) columnInfo.get("type");
        ParsedType result = parseType(dataType.toLowerCase());
        String lowercaseType = result.dataType;
        switch (lowercaseType) {
            case "bigint":
                return "bigint";
            case "tinyint":
            case "boolean":
                return "boolean";
            case "varchar":
            case "character varying":
                return StrUtil.format("varchar({})", result.length);
            case "date":
                return "date";
            case "int":
            case "integer":
                return "integer";
            case "decimal":
            case "numeric":
                return "real";
            case "text":
                return "text";
            case "time without time zone":
                return "time";
            case "timestamp":
            case "timestamp without time zone":
                return "timestamp";
            default:
                throw new RepositoryException("unsupported type:" + lowercaseType);
        }
    }
    @Override
    protected String getSQLForUpdateWhenPrepareId() {

        return "SELECT current_level from {} WHERE type_name = '{}'";
    }

    @Override
    protected void ensureIndexAndForeignKey(UserContext ctx) {
    }

    @Override
    protected void ensure(
            UserContext ctx, List<Map<String, Object>> tableInfo, String table, List<SQLColumn> columns) {
        tableInfo = CollStreamUtil.toList(tableInfo, CaseInsensitiveMap::new);
        super.ensure(ctx, tableInfo, table, columns);
    }

    protected String getPureColumnName(String columnName) {
        return StrUtil.unWrap(columnName, '`');
    }

    @Override
    protected String findTableColumnsSql(DataSource dataSource, String table) {

        return String.format(
                    //"SELECT name FROM sqlite_master WHERE type='table' AND name='%s'",
                    "PRAGMA table_info(%s)",
                    table);

    }
}
