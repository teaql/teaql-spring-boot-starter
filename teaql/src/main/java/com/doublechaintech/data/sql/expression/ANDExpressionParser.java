package com.doublechaintech.data.sql.expression;

import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;
import com.doublechaintech.data.UserContext;
import com.doublechaintech.data.criteria.AND;
import com.doublechaintech.data.sql.SQLColumnResolver;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ANDExpressionParser implements SQLExpressionParser<AND> {

  @Override
  public Class<AND> type() {
    return AND.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      AND expression,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    List<Expression> expressions = expression.getExpressions();
    List<String> subs = new ArrayList<>();
    for (Expression sub : expressions) {
      String sql = ExpressionHelper.toSql(userContext, sub, idTable, parameters, sqlColumnResolver);
      if (SearchCriteria.FALSE.equalsIgnoreCase(sql)) {
        return SearchCriteria.FALSE;
      }
      if (SearchCriteria.TRUE.equalsIgnoreCase(sql)) {
        continue;
      }
      subs.add(sql);
    }
    return subs.stream().map(sub -> "(" + sub + ")").collect(Collectors.joining(" AND "));
  }
}
