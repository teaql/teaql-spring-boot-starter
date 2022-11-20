package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class Contain extends TwoOperatorCriteria implements SearchCriteria {
  public Contain(Expression left, Expression right) {
    super(Operator.CONTAIN, left, right);
  }
}
