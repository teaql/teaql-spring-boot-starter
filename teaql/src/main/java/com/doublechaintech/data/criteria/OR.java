package com.doublechaintech.data.criteria;

import com.doublechaintech.data.FunctionApply;
import com.doublechaintech.data.PropertyAware;
import com.doublechaintech.data.SearchCriteria;

public class OR extends FunctionApply implements SearchCriteria, PropertyAware {
  public OR(SearchCriteria... pSubs) {
    super(LogicOperator.OR, pSubs);
  }
}
