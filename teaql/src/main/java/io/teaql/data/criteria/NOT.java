package io.teaql.data.criteria;

import io.teaql.data.FunctionApply;
import io.teaql.data.SearchCriteria;

public class NOT extends FunctionApply implements SearchCriteria {
  public NOT(SearchCriteria sub) {
    super(LogicOperator.NOT, sub);
  }
}
