package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.FunctionApply;
import com.doublechaintech.data.SearchCriteria;

public class Between extends FunctionApply implements SearchCriteria {
    public Between(Expression expression1, Expression expression2, Expression expression3) {
        super(Operator.BETWEEN, expression1, expression2, expression3);
    }
}
