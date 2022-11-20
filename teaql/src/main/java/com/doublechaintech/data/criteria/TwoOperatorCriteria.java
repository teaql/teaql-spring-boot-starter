package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.FunctionApply;
import com.doublechaintech.data.PropertyFunction;
import com.doublechaintech.data.SearchCriteria;

public class TwoOperatorCriteria extends FunctionApply implements SearchCriteria {
  public TwoOperatorCriteria(PropertyFunction operator, Expression left, Expression right) {
    super(operator, left, right);
  }
}
