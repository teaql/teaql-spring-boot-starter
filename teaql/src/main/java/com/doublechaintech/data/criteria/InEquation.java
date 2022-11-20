package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class InEquation extends TwoOperatorCriteria implements SearchCriteria {

  public InEquation(Expression left, Expression right) {
    super(Operator.NOT_EQUAL, left, right);
  }
}
