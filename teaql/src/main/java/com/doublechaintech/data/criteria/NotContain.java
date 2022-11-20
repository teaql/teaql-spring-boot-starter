package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class NotContain extends TwoOperatorCriteria implements SearchCriteria {
  public NotContain(Expression left, Expression right) {
    super(Operator.NOT_CONTAIN, left, right);
  }
}
