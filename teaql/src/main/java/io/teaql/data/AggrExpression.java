package io.teaql.data;

import io.teaql.data.sql.AggrFunction;

public class AggrExpression extends FunctionApply {
  public AggrExpression(AggrFunction operator, Expression expression) {
    super(operator, expression);
  }
}
