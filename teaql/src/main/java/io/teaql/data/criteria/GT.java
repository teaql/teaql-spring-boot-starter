package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class GT extends TwoOperatorCriteria implements SearchCriteria {
  public GT(Expression left, Expression right) {
    super(Operator.GREATER_THAN, left, right);
  }
}
