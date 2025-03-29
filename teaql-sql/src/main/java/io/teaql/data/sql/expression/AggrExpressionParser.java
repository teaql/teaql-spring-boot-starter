package io.teaql.data.sql.expression;

import java.util.List;
import java.util.Map;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;

import io.teaql.data.AggrExpression;
import io.teaql.data.AggrFunction;
import io.teaql.data.Expression;
import io.teaql.data.PropertyFunction;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLRepository;

public class AggrExpressionParser implements SQLExpressionParser<AggrExpression> {

    @Override
    public Class<AggrExpression> type() {
        return AggrExpression.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            AggrExpression agg,
            String idTable,
            Map<String, Object> parameters,
            SQLRepository sqlColumnResolver) {
        PropertyFunction operator = agg.getOperator();
        if (!(operator instanceof AggrFunction)) {
            throw new RepositoryException("AggrExpression operator should be " + AggrFunction.class);
        }

        List<Expression> expressions = agg.getExpressions();
        if (CollectionUtil.size(expressions) != 1) {
            throw new RepositoryException("AggrExpression operator should have 1 expression");
        }
        String sqlColumn =
                ExpressionHelper.toSql(
                        userContext, expressions.get(0), idTable, parameters, sqlColumnResolver);
        return genAggrSQL((AggrFunction) operator, sqlColumn);
    }

    public String genAggrSQL(AggrFunction operator, String sqlColumn) {
        AggrFunction aggrFunction = operator;
        switch (aggrFunction) {
            case SELF:
                return sqlColumn;
            case MIN:
                return StrUtil.format("min({})", sqlColumn);
            case MAX:
                return StrUtil.format("max({})", sqlColumn);
            case SUM:
                return StrUtil.format("sum({})", sqlColumn);
            case COUNT:
                return StrUtil.format("count({})", sqlColumn);
            case AVG:
                return StrUtil.format("avg({})", sqlColumn);
            case STDDEV:
                return StrUtil.format("stddev({})", sqlColumn);
            case STDDEV_POP:
                return StrUtil.format("stddev_pop({})", sqlColumn);
            case VAR_SAMP:
                return StrUtil.format("var_samp({})", sqlColumn);
            case VAR_POP:
                return StrUtil.format("var_pop({})", sqlColumn);
            case GBK:
                return StrUtil.format("convert_to({},'GBK')", sqlColumn);
        }
        throw new RepositoryException("unsupported agg function:" + aggrFunction);
    }
}
