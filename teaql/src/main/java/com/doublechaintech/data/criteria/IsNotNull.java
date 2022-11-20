package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;

public class IsNotNull extends OneOperatorCriteria implements SearchCriteria {
  public IsNotNull(Expression expressions) {
    super(Operator.IS_NOT_NULL, expressions);
  }
}
