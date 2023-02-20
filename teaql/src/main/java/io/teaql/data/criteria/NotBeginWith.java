package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class NotBeginWith extends TwoOperatorCriteria implements SearchCriteria {
  public NotBeginWith(Expression left, Expression right) {
    super(Operator.NOT_BEGIN_WITH, left, right);
  }
}
