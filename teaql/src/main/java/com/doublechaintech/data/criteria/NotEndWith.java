package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class NotEndWith extends TwoOperatorCriteria implements SearchCriteria {
  public NotEndWith(Expression left, Expression right) {
    super(Operator.NOT_END_WITH, left, right);
  }
}
