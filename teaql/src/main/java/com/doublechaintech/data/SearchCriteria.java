package com.doublechaintech.data;

import cn.hutool.core.collection.ListUtil;
import com.doublechaintech.data.criteria.AND;
import com.doublechaintech.data.criteria.OR;

import java.util.List;

public interface SearchCriteria extends Expression {
  String TRUE = "true";
  String FALSE = "false";

  static SearchCriteria and(SearchCriteria... sub) {
    return new AND(sub);
  }

  static SearchCriteria or(SearchCriteria... sub) {
    return new OR(sub);
  }

  static SearchCriteria not(SearchCriteria sub) {
    return new NOT(sub);
  }



  class NOT implements SearchCriteria, PropertyAware {
    private SearchCriteria inner;

    public SearchCriteria getInner() {
      return inner;
    }

    public void setInner(SearchCriteria pInner) {
      inner = pInner;
    }

    public NOT(SearchCriteria pInner) {
      inner = pInner;
    }

    @Override
    public List<String> properties(UserContext ctx) {
      if (inner != null) {
        return inner.properties(ctx);
      }
      return ListUtil.empty();
    }
  }


}
