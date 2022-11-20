package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class EQ extends TwoOperatorCriteria implements SearchCriteria {

  public EQ(Expression left, Expression right) {
    super(Operator.EQUAL, left, right);
  }
}
