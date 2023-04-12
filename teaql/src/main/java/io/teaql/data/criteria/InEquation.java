package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class InEquation extends TwoOperatorCriteria implements SearchCriteria {

  public InEquation(Expression left, Expression right) {
    super(Operator.NOT_EQUAL, left, right);
  }
}
