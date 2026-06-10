package io.teaql.data.sql.expression;

import java.util.Map;

import io.teaql.data.utils.StrUtil;

import io.teaql.data.FunctionApply;
import io.teaql.data.PropertyFunction;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.Operator;
import io.teaql.data.sql.SQLRepository;
import io.teaql.data.sql.SQLColumnResolver;
public class FunctionApplyParser implements SQLExpressionParser<FunctionApply> {
    @Override
    public Class<FunctionApply> type() {
        return FunctionApply.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            FunctionApply expression,
            String idTable,
            Map<String, Object> parameters,
            SQLColumnResolver sqlColumnResolver) {
        PropertyFunction operator = expression.getOperator();
        if (operator == Operator.SOUNDS_LIKE) {
            return StrUtil.format(
                    "SOUNDEX({})",
                    ExpressionHelper.toSql(
                            userContext, expression.first(), idTable, parameters, sqlColumnResolver));
        }
        throw new RepositoryException("unexpected operator:" + operator);
    }
}
