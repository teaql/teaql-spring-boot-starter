package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class IN extends TwoOperatorCriteria implements SearchCriteria {
    public IN(Expression left, Expression right) {
        super(Operator.IN, left, right);
    }
}
