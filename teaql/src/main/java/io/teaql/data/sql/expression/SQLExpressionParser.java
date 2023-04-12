package io.teaql.data.sql.expression;

import io.teaql.data.Expression;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLColumnResolver;

import java.util.Map;

public interface SQLExpressionParser<T extends Expression> {
  default Class<T> type() {
    return null;
  }

  default String toSql(
      UserContext userContext,
      T expression,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    return toSql(userContext, expression, parameters, sqlColumnResolver);
  }

  default String toSql(
      UserContext userContext,
      T expression,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    throw new RepositoryException("尚未实现");
  }

  default String nextPropertyKey(Map<String, Object> parameters, String propertyName) {
    while (parameters.containsKey(propertyName)) {
      propertyName = genNextKey(propertyName);
    }
    return propertyName;
  }

  default String genNextKey(String key) {
    char c = key.charAt(key.length() - 1);
    if (!Character.isDigit(c)) {
      return key + "0";
    } else {
      return key.substring(0, key.length() - 1) + (char) (c + 1);
    }
  }
}
