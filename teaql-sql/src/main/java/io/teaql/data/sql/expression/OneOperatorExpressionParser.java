package io.teaql.data.sql.expression;

import java.util.List;
import java.util.Map;

import io.teaql.data.utils.CollectionUtil;
import io.teaql.data.utils.StrUtil;

import io.teaql.data.Expression;
import io.teaql.data.PropertyFunction;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.OneOperatorCriteria;
import io.teaql.data.criteria.Operator;
import io.teaql.data.sql.SQLRepository;
import io.teaql.data.sql.SQLColumnResolver;
public class OneOperatorExpressionParser implements SQLExpressionParser<OneOperatorCriteria> {
    @Override
    public Class<OneOperatorCriteria> type() {
        return OneOperatorCriteria.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            OneOperatorCriteria criteria,
            String idTable,
            Map<String, Object> parameters,
            SQLColumnResolver sqlColumnResolver) {
        List<Expression> expressions = criteria.getExpressions();
        PropertyFunction operator = criteria.getOperator();
        if (!(operator instanceof Operator)) {
            throw new RepositoryException("unsupported operator:" + operator);
        }
        if (CollectionUtil.size(expressions) != 1) {
            throw new RepositoryException(operator + " should have one expression");
        }
        Expression left = expressions.get(0);
        String leftSQL =
                ExpressionHelper.toSql(userContext, left, idTable, parameters, sqlColumnResolver);
        return StrUtil.format("{} {}", leftSQL, getOp((Operator) operator));
    }

    private Object getOp(Operator operator) {
        switch (operator) {
            case IS_NULL:
                return "IS NULL";
            case IS_NOT_NULL:
                return "IS NOT NULL";
            default:
                throw new RepositoryException("unsupported operator:" + operator);
        }
    }
}
