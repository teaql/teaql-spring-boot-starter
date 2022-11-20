package com.doublechaintech.data.criteria;

import com.doublechaintech.data.FunctionApply;
import com.doublechaintech.data.SearchCriteria;

public class NOT extends FunctionApply implements SearchCriteria {
  public NOT(SearchCriteria sub) {
    super(LogicOperator.NOT, sub);
  }
}
