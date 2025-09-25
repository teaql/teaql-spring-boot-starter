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
                        "ALTER TABLE {} MODIFY COLUMN {} {}",
                        column.getTableName(),
                        column.getColumnName(),
                        column.getType());
        return alterColumnSql;
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
}
