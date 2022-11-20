package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class GT extends TwoOperatorCriteria implements SearchCriteria {
  public GT(Expression left, Expression right) {
    super(Operator.GREATER_THAN, left, right);
  }
}
