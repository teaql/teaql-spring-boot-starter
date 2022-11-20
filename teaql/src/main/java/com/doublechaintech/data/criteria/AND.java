package com.doublechaintech.data.criteria;

import com.doublechaintech.data.FunctionApply;
import com.doublechaintech.data.PropertyAware;
import com.doublechaintech.data.SearchCriteria;
import com.doublechaintech.data.criteria.LogicOperator;

public class AND extends FunctionApply implements SearchCriteria, PropertyAware {
  public AND(SearchCriteria... pSubs) {
    super(LogicOperator.AND, pSubs);
  }
}
