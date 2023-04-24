package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class GTE extends TwoOperatorCriteria implements SearchCriteria {
  public GTE(Expression left, Expression right) {
    super(Operator.GREATER_THAN_OR_EQUAL, left, right);
  }
}
