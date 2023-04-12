package io.teaql.data.criteria;

import io.teaql.data.FunctionApply;
import io.teaql.data.PropertyAware;
import io.teaql.data.SearchCriteria;

public class OR extends FunctionApply implements SearchCriteria, PropertyAware {
  public OR(SearchCriteria... pSubs) {
    super(LogicOperator.OR, pSubs);
  }
}
