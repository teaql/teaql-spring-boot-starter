package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class BeginWith extends TwoOperatorCriteria implements SearchCriteria {
    public BeginWith(Expression left, Expression right) {
        super(Operator.BEGIN_WITH, left, right);
    }
}
