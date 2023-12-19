package io.teaql.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import io.teaql.data.*;
import io.teaql.data.criteria.Operator;
import io.teaql.data.sql.SQLRepository;
import java.util.Map;

public class FunctionApplyParser implements SQLExpressionParser<FunctionApply> {
  @Override
  public Class<FunctionApply> type() {
    return FunctionApply.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      FunctionApply expression,
      String idTable,
      Map<String, Object> parameters,
      SQLRepository sqlRepository) {
    PropertyFunction operator = expression.getOperator();
    if (operator == Operator.SOUNDS_LIKE) {
      return StrUtil.format(
          "SOUNDEX({})",
          ExpressionHelper.toSql(
              userContext, expression.first(), idTable, parameters, sqlRepository));
    }
    throw new RepositoryException("unexpected operator:" + operator);
  }
}
