package io.teaql.data.sql.expression;

import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.OR;
import io.teaql.data.sql.SQLColumnResolver;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ORExpressionParser implements SQLExpressionParser<OR> {

  @Override
  public Class<OR> type() {
    return OR.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      OR expression,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    List<Expression> expressions = expression.getExpressions();
    List<String> subs = new ArrayList<>();
    for (Expression sub : expressions) {
      String sql = ExpressionHelper.toSql(userContext, sub, idTable, parameters, sqlColumnResolver);
      if (SearchCriteria.FALSE.equalsIgnoreCase(sql)) {
        continue;
      }
      if (SearchCriteria.TRUE.equalsIgnoreCase(sql)) {
        return SearchCriteria.TRUE;
      }
      subs.add(sql);
    }
    return subs.stream().map(sub -> "(" + sub + ")").collect(Collectors.joining(" OR "));
  }
}
