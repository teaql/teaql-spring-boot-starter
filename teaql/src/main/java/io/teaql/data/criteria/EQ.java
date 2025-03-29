package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class EQ extends TwoOperatorCriteria implements SearchCriteria {

    public EQ(Expression left, Expression right) {
        super(Operator.EQUAL, left, right);
    }
}
