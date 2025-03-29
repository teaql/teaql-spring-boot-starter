package io.teaql.data;

public class AggrExpression extends FunctionApply {
  public AggrExpression(AggrFunction operator, Expression expression) {
    super(operator, expression);
  }
}
