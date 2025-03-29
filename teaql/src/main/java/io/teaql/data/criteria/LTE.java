package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class LTE extends TwoOperatorCriteria implements SearchCriteria {
    public LTE(Expression left, Expression right) {
        super(Operator.LESS_THAN_OR_EQUAL, left, right);
    }
}
