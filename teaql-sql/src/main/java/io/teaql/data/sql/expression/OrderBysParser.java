package io.teaql.data.sql.expression;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import io.teaql.data.OrderBy;
import io.teaql.data.OrderBys;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLRepository;

public class OrderBysParser implements SQLExpressionParser<OrderBys> {
    @Override
    public Class<OrderBys> type() {
        return OrderBys.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            OrderBys expression,
            String idTable,
            Map<String, Object> parameters,
            SQLRepository sqlColumnResolver) {
        List<OrderBy> orderBys = expression.getOrderBys();
        if (orderBys.isEmpty()) {
            return null;
        }
        return orderBys.stream()
                .map(
                        order ->
                                ExpressionHelper.toSql(userContext, order, idTable, parameters, sqlColumnResolver))
                .collect(Collectors.joining(", ", "ORDER BY ", ""));
    }
}
