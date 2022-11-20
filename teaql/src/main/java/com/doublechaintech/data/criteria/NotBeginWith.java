package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class NotBeginWith extends TwoOperatorCriteria implements SearchCriteria {
  public NotBeginWith(Expression left, Expression right) {
    super(Operator.NOT_BEGIN_WITH, left, right);
  }
}
