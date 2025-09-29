package io.teaql.data.mysql;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.map.CaseInsensitiveMap;
import cn.hutool.core.util.StrUtil;

import io.teaql.data.Entity;
import io.teaql.data.SearchRequest;
import io.teaql.data.Slice;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLRepository;

public class MysqlRepository<T extends Entity> extends SQLRepository<T> {
    public MysqlRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
        super(entityDescriptor, dataSource);
        registerExpressionParser(MysqlAggrExpressionParser.class);
        registerExpressionParser(MysqlParameterParser.class);
        registerExpressionParser(MysqlTwoOperatorExpressionParser.class);
    }

    protected String generateAlterColumnSQL(UserContext ctx, SQLColumn column) {
        String alterColumnSql =
                StrUtil.format(
                        "ALTER TABLE {} MODIFY COLUMN {}  {}",
                        column.getTableName(),
                        column.getColumnName(),
                        column.getType());
        return alterColumnSql;
    }
/*

SELECT
    tc.CONSTRAINT_NAME AS name,
    tc.TABLE_NAME AS tableName,
    kcu.COLUMN_NAME AS columnName,
    kcu.REFERENCED_TABLE_NAME AS fTableName,
    kcu.REFERENCED_COLUMN_NAME AS fColumnName
FROM
    information_schema.TABLE_CONSTRAINTS AS tc
JOIN information_schema.KEY_COLUMN_USAGE AS kcu
    ON tc.CONSTRAINT_NAME = kcu.CONSTRAINT_NAME
WHERE
    tc.CONSTRAINT_TYPE = 'FOREIGN KEY';

*/
    protected String fetchFKsSQL() {
        return """
            SELECT
                tc.CONSTRAINT_NAME AS name,
                tc.TABLE_NAME AS tableName,
                kcu.COLUMN_NAME AS columnName,
                kcu.REFERENCED_TABLE_NAME AS fTableName,
                kcu.REFERENCED_COLUMN_NAME AS fColumnName
            FROM
                information_schema.TABLE_CONSTRAINTS AS tc
            JOIN information_schema.KEY_COLUMN_USAGE AS kcu
                ON tc.CONSTRAINT_NAME = kcu.CONSTRAINT_NAME
            WHERE
                tc.CONSTRAINT_TYPE = 'FOREIGN KEY';
                """;
    }
    @Override
    protected void ensureIndexAndForeignKey(UserContext ctx) {
        super.ensureIndexAndForeignKey(ctx);


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
        try (Connection connection = dataSource.getConnection()) {
            String databaseName = connection.getCatalog();
            return String.format(
                    "select * from information_schema.columns where table_name = '%s' and table_schema = '%s'",
                    table, databaseName);
        }
        catch (SQLException pE) {
            throw new RuntimeException(pE);
        }
    }

    @Override
    public boolean canMixinSubQuery(UserContext userContext, SearchRequest subQuery) {
        Slice slice = subQuery.getSlice();
        return slice == null;
    }
}
