package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class InLarge extends TwoOperatorCriteria implements SearchCriteria {
  public InLarge(Expression left, Expression right) {
    super(Operator.IN_LARGE, left, right);
  }
}
