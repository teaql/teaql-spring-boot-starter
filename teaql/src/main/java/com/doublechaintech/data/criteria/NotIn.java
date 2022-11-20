package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class NotIn extends TwoOperatorCriteria implements SearchCriteria {
    public NotIn(Expression left, Expression right) {
        super(Operator.NOT_IN, left, right);
    }
}
