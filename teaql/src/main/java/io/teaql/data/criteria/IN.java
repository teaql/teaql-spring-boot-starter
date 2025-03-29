package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class IN extends TwoOperatorCriteria implements SearchCriteria {
  public IN(Expression left, Expression right) {
    super(Operator.IN, left, right);
  }
}
