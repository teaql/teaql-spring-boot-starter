package com.doublechaintech.data;

import java.util.List;

public class OrderBy implements Expression {
  private Expression expression;
  private String direction = "ASC";

  public OrderBy(AggrFunction function, String property, String direction) {
    this.expression = new AggrExpression(function, new PropertyReference(property));
    this.direction = direction;
  }

  public OrderBy(String property) {
    this(AggrFunction.SELF, property, "ASC");
  }

  public OrderBy(String property, String direction) {
    this(AggrFunction.SELF, property, direction);
  }

  public Expression getExpression() {
    return expression;
  }

  public void setExpression(Expression pExpression) {
    expression = pExpression;
  }

  public String getDirection() {
    return direction;
  }

  public void setDirection(String pDirection) {
    direction = pDirection;
  }

  @Override
  public List<String> properties(UserContext ctx) {
    return expression.properties(ctx);
  }
}
