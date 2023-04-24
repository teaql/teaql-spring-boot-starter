package io.teaql.data.criteria;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;

public class IsNotNull extends OneOperatorCriteria implements SearchCriteria {
  public IsNotNull(Expression expressions) {
    super(Operator.IS_NOT_NULL, expressions);
  }
}
