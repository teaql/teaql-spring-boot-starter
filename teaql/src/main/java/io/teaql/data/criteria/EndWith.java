package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class EndWith extends TwoOperatorCriteria implements SearchCriteria {
  public EndWith(Expression left, Expression right) {
    super(Operator.END_WITH, left, right);
  }
}
