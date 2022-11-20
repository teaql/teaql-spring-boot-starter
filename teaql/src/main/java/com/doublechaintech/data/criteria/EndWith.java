package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class EndWith extends TwoOperatorCriteria implements SearchCriteria {
  public EndWith(Expression left, Expression right) {
    super(Operator.END_WITH, left, right);
  }
}
