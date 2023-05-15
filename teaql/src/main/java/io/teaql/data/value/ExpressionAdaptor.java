package io.teaql.data.value;

import java.util.function.Function;

public class ExpressionAdaptor<T, E> implements Expression<T, E> {

  private Expression<T, E> expression;

  private ExpressionAdaptor(Expression<T, E> expression) {
    this.expression = expression;
  }

  @Override
  public E eval(T pT) {
    return expression.eval(pT);
  }

  @Override
  public <U> Expression<T, U> apply(Function<E, U> function) {
    return expression.apply(function);
  }

  @Override
  public T $getRoot() {
    return expression.$getRoot();
  }

  @Override
  public E eval() {
    return expression.eval();
  }

  @Override
  public boolean isNull() {
    return expression.isNull();
  }

  @Override
  public boolean isNotNull() {
    return expression.isNotNull();
  }

  @Override
  public boolean isEmpty() {
    return expression.isEmpty();
  }

  @Override
  public boolean isNotEmpty() {
    return expression.isNotEmpty();
  }

  @Override
  public void whenIsNull(Runnable function) {
    expression.whenIsNull(function);
  }

  @Override
  public void whenIsNotNull(Runnable function) {
    expression.whenIsNotNull(function);
  }

  @Override
  public void whenIsEmpty(Runnable function) {
    expression.whenIsEmpty(function);
  }

  @Override
  public void whenNotEmpty(Runnable function) {
    expression.whenNotEmpty(function);
  }
}
