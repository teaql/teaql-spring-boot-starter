package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class Contain extends TwoOperatorCriteria implements SearchCriteria {
  public Contain(Expression left, Expression right) {
    super(Operator.CONTAIN, left, right);
  }
}
