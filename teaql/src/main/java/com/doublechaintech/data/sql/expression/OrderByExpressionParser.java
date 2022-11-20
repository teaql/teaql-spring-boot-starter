package com.doublechaintech.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import com.doublechaintech.data.OrderBy;
import com.doublechaintech.data.UserContext;
import com.doublechaintech.data.sql.SQLColumnResolver;

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
      SQLColumnResolver sqlColumnResolver) {
    return StrUtil.format("{} {}", ExpressionHelper.toSql(userContext, expression.getExpression(), idTable, parameters, sqlColumnResolver), expression.getDirection());
  }
}
