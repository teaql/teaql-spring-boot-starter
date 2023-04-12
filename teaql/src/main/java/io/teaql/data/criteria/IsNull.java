package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class IsNull extends OneOperatorCriteria implements SearchCriteria {
  public IsNull(Expression expression) {
    super(Operator.IS_NULL, expression);
  }
}
