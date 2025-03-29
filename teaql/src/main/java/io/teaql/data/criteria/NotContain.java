package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class NotContain extends TwoOperatorCriteria implements SearchCriteria {
    public NotContain(Expression left, Expression right) {
        super(Operator.NOT_CONTAIN, left, right);
    }
}
