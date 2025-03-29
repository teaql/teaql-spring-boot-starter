package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.FunctionApply;
import io.teaql.data.SearchCriteria;

public class Between extends FunctionApply implements SearchCriteria {
    public Between(Expression expression1, Expression expression2, Expression expression3) {
        super(Operator.BETWEEN, expression1, expression2, expression3);
    }
}
