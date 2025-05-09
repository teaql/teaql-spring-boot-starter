package io.teaql.data.sql.expression;

import java.util.List;
import java.util.Map;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;

import io.teaql.data.Expression;
import io.teaql.data.PropertyFunction;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.Operator;
import io.teaql.data.criteria.TwoOperatorCriteria;
import io.teaql.data.sql.SQLRepository;

public class TwoOperatorExpressionParser implements SQLExpressionParser<TwoOperatorCriteria> {
    @Override
    public Class<TwoOperatorCriteria> type() {
        return TwoOperatorCriteria.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            TwoOperatorCriteria twoOperatorCriteria,
            String idTable,
            Map<String, Object> parameters,
            SQLRepository sqlColumnResolver) {
        List<Expression> expressions = twoOperatorCriteria.getExpressions();
        PropertyFunction operator = twoOperatorCriteria.getOperator();
        if (!(operator instanceof Operator)) {
            throw new RepositoryException("unsupported operator:" + operator);
        }
        if (CollectionUtil.size(expressions) != 2) {
            throw new RepositoryException(operator + " should have 2 expressions");
        }
        Expression left = twoOperatorCriteria.first();
        Expression right = twoOperatorCriteria.second();
        String leftSQL =
                ExpressionHelper.toSql(userContext, left, idTable, parameters, sqlColumnResolver);
        String rightSQL =
                ExpressionHelper.toSql(userContext, right, idTable, parameters, sqlColumnResolver);
        return StrUtil.format(
                "{} {} {}{}{}",
                leftSQL,
                getOp((Operator) operator),
                getPrefix((Operator) operator),
                rightSQL,
                getSuffix((Operator) operator));
    }

    public Object getSuffix(Operator operator) {
        switch (operator) {
            case IN:
            case NOT_IN:
            case IN_LARGE:
            case NOT_IN_LARGE:
                return ")";
            default:
                return "";
        }
    }

    public Object getPrefix(Operator operator) {
        switch (operator) {
            case IN:
            case NOT_IN:
            case IN_LARGE:
            case NOT_IN_LARGE:
                return "(";
            default:
                return "";
        }
    }

    public String getOp(Operator operator) {
        switch (operator) {
            case EQUAL:
                return "=";
            case NOT_EQUAL:
                return "<>";
            case CONTAIN:
            case BEGIN_WITH:
            case END_WITH:
                return "LIKE";
            case NOT_CONTAIN:
            case NOT_BEGIN_WITH:
            case NOT_END_WITH:
                return "NOT LIKE";
            case GREATER_THAN:
                return ">";
            case GREATER_THAN_OR_EQUAL:
                return ">=";
            case LESS_THAN:
                return "<";
            case LESS_THAN_OR_EQUAL:
                return "<=";
            case IN:
                return "IN";
            case IN_LARGE:
                return "= ANY";
            case NOT_IN:
                return "NOT IN";
            case NOT_IN_LARGE:
                return "<> ALL";
            default:
                throw new RepositoryException("unsupported operator:" + operator);
        }
    }
}
