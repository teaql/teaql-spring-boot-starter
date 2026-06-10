package io.teaql.data.sql.expression;

import java.util.Map;

import io.teaql.data.utils.StrUtil;

import io.teaql.data.OrderBy;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLRepository;
import io.teaql.data.sql.SQLColumnResolver;
public class OrderByExpressionParser implements SQLExpressionParser<OrderBy> {

    @Override
    public Class<OrderBy> type() {
        return OrderBy.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            OrderBy expression,
            String idTable,
            Map<String, Object> parameters,
            SQLColumnResolver sqlColumnResolver) {
        return StrUtil.format(
                "{} {}",
                ExpressionHelper.toSql(
                        userContext, expression.getExpression(), idTable, parameters, sqlColumnResolver),
                expression.getDirection());
    }
}
