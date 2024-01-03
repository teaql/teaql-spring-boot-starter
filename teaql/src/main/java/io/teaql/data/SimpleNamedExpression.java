package io.teaql.data;

import java.util.List;

public class SimpleNamedExpression implements Expression {
  String name;
  Expression expression;

  public SimpleNamedExpression(String name, Expression expression) {
    if (expression == null) {
      throw new RepositoryException("SimpleNamedExpression expression cannot be null");
    }
    this.name = name;
    this.expression = expression;
  }

  public SimpleNamedExpression(String propertyName) {
    this(propertyName, new PropertyReference(propertyName));
  }

  public String name() {
    return this.name;
  }

  public Expression getExpression() {
    return expression;
  }

  @Override
  public List<String> properties(UserContext ctx) {
    return expression.properties(ctx);
  }
}
