package io.teaql.data.sql.expression;

import java.util.Map;

import io.teaql.data.utils.StrUtil;

import io.teaql.data.Expression;
import io.teaql.data.SimpleNamedExpression;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLRepository;
import io.teaql.data.sql.SQLColumnResolver;
public class NamedExpressionParser implements SQLExpressionParser<SimpleNamedExpression> {
    @Override
    public Class<SimpleNamedExpression> type() {
        return SimpleNamedExpression.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            SimpleNamedExpression expression,
            String idTable,
            Map<String, Object> parameters,
            SQLColumnResolver sqlColumnResolver) {
        Expression inner = expression.getExpression();
        String sql = ExpressionHelper.toSql(userContext, inner, idTable, parameters, sqlColumnResolver);
        String name = expression.name();
        if (!name.toLowerCase().equals(name)) {
            name = StrUtil.wrap(name, "\"");
        }
        if (sql.equals(name)) {
            return sql;
        }
        return StrUtil.format("{} AS {}", sql, name);
    }
}
