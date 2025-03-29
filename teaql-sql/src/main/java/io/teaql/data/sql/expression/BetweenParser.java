package io.teaql.data.sql.expression;

import java.util.List;
import java.util.Map;

import cn.hutool.core.util.StrUtil;

import io.teaql.data.Expression;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.Between;
import io.teaql.data.sql.SQLRepository;

public class BetweenParser implements SQLExpressionParser<Between> {
    @Override
    public Class<Between> type() {
        return Between.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            Between expression,
            String idTable,
            Map<String, Object> parameters,
            SQLRepository sqlColumnResolver) {
        List<Expression> expressions = expression.getExpressions();
        Expression property = expressions.get(0);
        Expression lowValue = expressions.get(1);
        Expression highValue = expressions.get(2);
        return StrUtil.format(
                "{} BETWEEN {} AND {}",
                ExpressionHelper.toSql(userContext, property, idTable, parameters, sqlColumnResolver),
                ExpressionHelper.toSql(userContext, lowValue, idTable, parameters, sqlColumnResolver),
                ExpressionHelper.toSql(userContext, highValue, idTable, parameters, sqlColumnResolver));
    }
}
