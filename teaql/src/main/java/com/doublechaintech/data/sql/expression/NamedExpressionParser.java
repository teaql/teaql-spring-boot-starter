package com.doublechaintech.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SimpleNamedExpression;
import com.doublechaintech.data.UserContext;
import com.doublechaintech.data.sql.SQLColumnResolver;

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
