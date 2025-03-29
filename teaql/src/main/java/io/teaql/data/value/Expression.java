package io.teaql.data.value;

import java.util.function.Consumer;
import java.util.function.Function;

public interface Expression<E, T> {
  T eval(E e);

  default <U> Expression<E, U> apply(Function<T, U> function) {
    return new ExpressionAdaptor(this, function);
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

  default void whenIsNotNull(Consumer<T> consumer) {
    if (isNotNull() && consumer != null) {
      consumer.accept(eval());
    }
  }

  default void whenIsEmpty(Runnable function) {
    if (isEmpty() && function != null) {
      function.run();
    }
  }

  default void whenNotEmpty(Consumer<T> consumer) {
    if (isNotEmpty() && consumer != null) {
      consumer.accept(eval());
    }
  }

  default void whenNotEmpty(Runnable function) {
    if (isNotEmpty() && function != null) {
      function.run();
    }
  }
}
