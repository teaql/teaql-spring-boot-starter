package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.FunctionApply;
import io.teaql.data.SearchCriteria;

public class OneOperatorCriteria extends FunctionApply implements SearchCriteria {
    public OneOperatorCriteria(Operator operator, Expression expression) {
        super(operator, expression);
    }
}
