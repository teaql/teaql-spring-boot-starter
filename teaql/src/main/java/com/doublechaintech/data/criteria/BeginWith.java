package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class BeginWith extends TwoOperatorCriteria implements SearchCriteria {
  public BeginWith(Expression left, Expression right) {
    super(Operator.BEGIN_WITH, left, right);
  }
}
