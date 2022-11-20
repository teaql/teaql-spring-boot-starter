package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class LT extends TwoOperatorCriteria implements SearchCriteria {
  public LT(Expression left, Expression right) {
    super(Operator.LESS_THAN, left, right);
  }
}
