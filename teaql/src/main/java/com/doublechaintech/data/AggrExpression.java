package com.doublechaintech.data;

import com.doublechaintech.data.sql.AggrFunction;

public class AggrExpression extends FunctionApply {
  public AggrExpression(AggrFunction operator, Expression expression) {
    super(operator, expression);
  }
}
