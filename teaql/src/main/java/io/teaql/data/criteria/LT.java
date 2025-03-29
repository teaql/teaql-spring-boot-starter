package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class LT extends TwoOperatorCriteria implements SearchCriteria {
    public LT(Expression left, Expression right) {
        super(Operator.LESS_THAN, left, right);
    }
}
