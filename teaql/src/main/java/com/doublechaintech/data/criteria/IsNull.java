package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class IsNull extends OneOperatorCriteria implements SearchCriteria {
  public IsNull(Expression expression) {
    super(Operator.IS_NULL, expression);
  }
}
