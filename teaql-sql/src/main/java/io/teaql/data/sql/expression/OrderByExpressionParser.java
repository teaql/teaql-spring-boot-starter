package io.teaql.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import io.teaql.data.OrderBy;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLRepository;
import java.util.Map;

public class OrderByExpressionParser implements SQLExpressionParser<OrderBy> {

  @Override
  public Class<OrderBy> type() {
    return OrderBy.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      OrderBy expression,
      String idTable,
      Map<String, Object> parameters,
      SQLRepository sqlColumnResolver) {
    return StrUtil.format(
        "{} {}",
        ExpressionHelper.toSql(
            userContext, expression.getExpression(), idTable, parameters, sqlColumnResolver),
        expression.getDirection());
  }
}
