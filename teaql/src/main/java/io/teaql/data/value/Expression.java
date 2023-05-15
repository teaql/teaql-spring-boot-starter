package io.teaql.data.value;

import java.util.function.Function;

public interface Expression<E, T> {
  T eval(E e);

  default <U> Expression<E, U> apply(Function<T, U> function) {
    return new Expression<>() {
      @Override
      public U eval(E e) {
        T eval = Expression.this.eval(e);
        if (function == null) {
          return null;
        }
        return function.apply(eval);
      }

      @Override
      public E $getRoot() {
        return Expression.super.$getRoot();
      }
    };
  }

  default E $getRoot() {
    return null;
  }

  default T eval() {
    return eval($getRoot());
  }

  default boolean isNull() {
    return null == eval();
  }

  default boolean isNotNull() {
    return null != eval();
  }

  default boolean isEmpty() {
    return cn.hutool.core.util.ObjectUtil.isEmpty(eval());
  }

  default boolean isNotEmpty() {
    return cn.hutool.core.util.ObjectUtil.isNotEmpty(eval());
  }

  default void whenIsNull(Runnable function) {
    if (isNull() && function != null) {
      function.run();
    }
  }

  default void whenIsNotNull(Runnable function) {
    if (isNotNull() && function != null) {
      function.run();
    }
  }

  default void whenIsEmpty(Runnable function) {
    if (isEmpty() && function != null) {
      function.run();
    }
  }

  default void whenNotEmpty(Runnable function) {
    if (isNotEmpty() && function != null) {
      function.run();
    }
  }
}
