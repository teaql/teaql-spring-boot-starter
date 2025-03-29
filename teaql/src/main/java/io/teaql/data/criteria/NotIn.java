package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class NotIn extends TwoOperatorCriteria implements SearchCriteria {
  public NotIn(Expression left, Expression right) {
    super(Operator.NOT_IN, left, right);
  }
}
