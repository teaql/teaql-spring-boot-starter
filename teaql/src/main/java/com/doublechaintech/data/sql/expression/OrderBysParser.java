package com.doublechaintech.data.sql.expression;

import com.doublechaintech.data.OrderBy;
import com.doublechaintech.data.OrderBys;
import com.doublechaintech.data.UserContext;
import com.doublechaintech.data.sql.SQLColumnResolver;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class OrderBysParser implements SQLExpressionParser<OrderBys>{
    @Override
    public Class<OrderBys> type() {
        return OrderBys.class;
    }

    @Override
    public String toSql(UserContext userContext, OrderBys expression, String idTable, Map<String, Object> parameters, SQLColumnResolver sqlColumnResolver) {
        List<OrderBy> orderBys = expression.getOrderBys();
        if (orderBys.isEmpty()){
            return null;
        }
        return orderBys.stream().map(order -> ExpressionHelper.toSql(userContext, order, idTable, parameters, sqlColumnResolver))
                .collect(Collectors.joining(",", "ORDER BY ", ""));
    }
}
