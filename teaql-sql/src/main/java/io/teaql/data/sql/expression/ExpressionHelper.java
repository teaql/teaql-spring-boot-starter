package io.teaql.data.sql.expression;

import cn.hutool.core.util.ClassUtil;
import cn.hutool.core.util.ReflectUtil;
import io.teaql.data.Expression;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLColumnResolver;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class ExpressionHelper {

  public static Map<Class, SQLExpressionParser> expressionParsers = new ConcurrentHashMap<>();

  static {
    Set<Class<?>> parsers =
        ClassUtil.scanPackageBySuper(
            ExpressionHelper.class.getPackageName(), SQLExpressionParser.class);
    for (Class<?> parser : parsers) {
      if (!parser.isInterface()) {
        SQLExpressionParser o = (SQLExpressionParser) ReflectUtil.newInstance(parser);
        Class type = o.type();
        if (type != null) {
          expressionParsers.put(type, o);
        }
      }
    }
  }

  public static String toSql(
      UserContext userContext,
      Expression expression,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver columnProvider) {
    if (expression == null) {
      return null;
    }

    if (expression instanceof SQLExpressionParser) {
      return ((SQLExpressionParser) expression)
          .toSql(userContext, expression, idTable, parameters, columnProvider);
    }

    Class expressionClass = expression.getClass();
    SQLExpressionParser parser = null;
    while (expressionClass != null) {
      parser = expressionParsers.get(expressionClass);
      if (parser != null) {
        break;
      }
      expressionClass = expressionClass.getSuperclass();
    }
    if (parser == null) {
      throw new RepositoryException("目前还不支持表达式类型:" + expression.getClass());
    }
    return parser.toSql(userContext, expression, idTable, parameters, columnProvider);
  }
}
