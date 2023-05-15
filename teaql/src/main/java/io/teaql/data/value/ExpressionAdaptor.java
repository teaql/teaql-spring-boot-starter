package io.teaql.data.value;

import java.util.function.Function;

public class ExpressionAdaptor<T, E, U> implements Expression<T, U> {
  private Expression<T, E> expression;
  private Function<E, U> function;

  public ExpressionAdaptor(Expression<T, E> pExpression, Function<E, U> pFunction) {
    expression = pExpression;
    function = pFunction;
  }

  @Override
  public U eval(T pT) {
    E eval = expression.eval(pT);
    if (eval == null) {
      return null;
    }
    return function.apply(eval);
  }

  @Override
  public T $getRoot() {
    return expression.$getRoot();
  }
}
