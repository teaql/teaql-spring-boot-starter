package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.FunctionApply;
import com.doublechaintech.data.SearchCriteria;

public class OneOperatorCriteria extends FunctionApply implements SearchCriteria {
  public OneOperatorCriteria(Operator operator, Expression expression) {
    super(operator, expression);
  }
}
