package io.teaql.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import io.teaql.data.Expression;
import io.teaql.data.SimpleNamedExpression;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLColumnResolver;

import java.util.Map;

public class NamedExpressionParser implements SQLExpressionParser<SimpleNamedExpression> {
  @Override
  public Class<SimpleNamedExpression> type() {
    return SimpleNamedExpression.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      SimpleNamedExpression expression,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    Expression inner = expression.getExpression();
    String sql = ExpressionHelper.toSql(userContext, inner, idTable, parameters, sqlColumnResolver);
    return StrUtil.format("{} AS {}", sql, expression.name());
  }
}
