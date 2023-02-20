package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class NotEndWith extends TwoOperatorCriteria implements SearchCriteria {
  public NotEndWith(Expression left, Expression right) {
    super(Operator.NOT_END_WITH, left, right);
  }
}
