package io.teaql.data.sql.expression;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;
import io.teaql.data.Expression;
import io.teaql.data.SearchCriteria;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.NOT;
import io.teaql.data.sql.SQLColumnResolver;
import io.teaql.data.sql.SQLRepository;

import java.util.List;
import java.util.Map;

public class NOTExpressionParser implements SQLExpressionParser<NOT> {

  @Override
  public Class<NOT> type() {
    return NOT.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      NOT expression,
      String idTable,
      Map<String, Object> parameters,
      SQLRepository sqlColumnResolver) {
    List<Expression> expressions = expression.getExpressions();
    Expression sub = CollectionUtil.getFirst(expressions);
    if (sub == null) {
      return SearchCriteria.TRUE;
    }
    String subSql =
        ExpressionHelper.toSql(userContext, sub, idTable, parameters, sqlColumnResolver);
    if (SearchCriteria.TRUE.equalsIgnoreCase(subSql)) {
      return SearchCriteria.FALSE;
    }

    if (SearchCriteria.FALSE.equalsIgnoreCase(subSql)) {
      return SearchCriteria.TRUE;
    }
    return StrUtil.format("NOT ({})", subSql);
  }
}
