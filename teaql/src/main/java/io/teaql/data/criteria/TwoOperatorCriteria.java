package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.FunctionApply;
import io.teaql.data.PropertyFunction;
import io.teaql.data.SearchCriteria;

public class TwoOperatorCriteria extends FunctionApply implements SearchCriteria {
    public TwoOperatorCriteria(PropertyFunction operator, Expression left, Expression right) {
        super(operator, left, right);
    }
}
