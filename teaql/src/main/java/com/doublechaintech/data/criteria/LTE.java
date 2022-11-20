package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class LTE extends TwoOperatorCriteria implements SearchCriteria {
  public LTE(Expression left, Expression right) {
    super(Operator.LESS_THAN_OR_EQUAL, left, right);
  }
}
