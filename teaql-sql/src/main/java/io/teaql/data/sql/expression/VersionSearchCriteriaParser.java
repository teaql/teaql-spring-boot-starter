package io.teaql.data.sql.expression;

import io.teaql.data.SearchCriteria;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.VersionSearchCriteria;
import io.teaql.data.sql.SQLColumnResolver;
import java.util.Map;

public class VersionSearchCriteriaParser implements SQLExpressionParser<VersionSearchCriteria> {
  public Class<VersionSearchCriteria> type() {
    return VersionSearchCriteria.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      VersionSearchCriteria expression,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    SearchCriteria searchCriteria = expression.getSearchCriteria();
    return ExpressionHelper.toSql(
        userContext, searchCriteria, idTable, parameters, sqlColumnResolver);
  }
}
