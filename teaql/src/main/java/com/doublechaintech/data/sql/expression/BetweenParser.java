package com.doublechaintech.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import com.doublechaintech.data.Expression;
import com.doublechaintech.data.UserContext;
import com.doublechaintech.data.criteria.Between;
import com.doublechaintech.data.sql.SQLColumnResolver;

import java.util.List;
import java.util.Map;

public class BetweenParser implements SQLExpressionParser<Between> {
  @Override
  public Class<Between> type() {
    return Between.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      Between expression,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    List<Expression> expressions = expression.getExpressions();
    Expression property = expressions.get(0);
    Expression lowValue = expressions.get(1);
    Expression highValue = expressions.get(2);
    return StrUtil.format(
        "{} BETWEEN {} AND {}",
        ExpressionHelper.toSql(userContext, property, idTable, parameters, sqlColumnResolver),
        ExpressionHelper.toSql(userContext, lowValue, idTable, parameters, sqlColumnResolver),
        ExpressionHelper.toSql(userContext, highValue, idTable, parameters, sqlColumnResolver));
  }
}
