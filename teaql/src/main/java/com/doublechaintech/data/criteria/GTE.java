package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class GTE extends TwoOperatorCriteria implements SearchCriteria {
  public GTE(Expression left, Expression right) {
    super(Operator.GREATER_THAN_OR_EQUAL, left, right);
  }
}
