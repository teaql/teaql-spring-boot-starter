package io.teaql.data.criteria;

import io.teaql.data.FunctionApply;
import io.teaql.data.PropertyAware;
import io.teaql.data.SearchCriteria;

public class AND extends FunctionApply implements SearchCriteria, PropertyAware {
    public AND(SearchCriteria... pSubs) {
        super(LogicOperator.AND, pSubs);
    }
}
