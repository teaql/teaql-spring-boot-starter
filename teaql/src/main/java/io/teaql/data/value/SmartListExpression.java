package io.teaql.data.value;

import io.teaql.data.BaseEntity;
import io.teaql.data.SmartList;

public interface SmartListExpression<T, U extends BaseEntity> extends Expression<T, SmartList<U>> {
  default Expression<T, Integer> size() {
    return apply(list -> list.size());
  }

  default Expression<T, U> first() {
    return apply(list -> list.get(0));
  }

  default Expression<T, U> get(int index) {
    return apply(
        list -> {
          if (index < 0 || index > list.size() - 1) {
            return null;
          }
          return list.get(index);
        });
  }
}
