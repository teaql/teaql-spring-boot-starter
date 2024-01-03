package io.teaql.data.sql.expression;

import io.teaql.data.Expression;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLRepository;
import java.util.Map;

public class ExpressionHelper {

  public static String toSql(
      UserContext userContext,
      Expression expression,
      String idTable,
      Map<String, Object> parameters,
      SQLRepository sqlRepository) {
    if (expression == null) {
      return null;
    }
    if (expression instanceof SQLExpressionParser) {
      return ((SQLExpressionParser) expression)
          .toSql(userContext, expression, idTable, parameters, sqlRepository);
    }

    Class expressionClass = expression.getClass();
    SQLExpressionParser parser = null;

    Map<Class, SQLExpressionParser> expressionParsers = sqlRepository.getExpressionParsers();
    while (expressionClass != null) {
      parser = expressionParsers.get(expressionClass);
      if (parser != null) {
        break;
      }
      expressionClass = expressionClass.getSuperclass();
    }
    if (parser == null) {
      throw new RepositoryException("no parse for expression type:" + expression.getClass());
    }
    return parser.toSql(userContext, expression, idTable, parameters, sqlRepository);
  }
}
