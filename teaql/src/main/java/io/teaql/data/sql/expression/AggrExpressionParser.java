package io.teaql.data.sql.expression;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;
import io.teaql.data.*;
import io.teaql.data.sql.AggrFunction;
import io.teaql.data.sql.SQLColumnResolver;

import java.util.List;
import java.util.Map;

public class AggrExpressionParser implements SQLExpressionParser<AggrExpression> {

  @Override
  public Class<AggrExpression> type() {
    return AggrExpression.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      AggrExpression agg,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    PropertyFunction operator = agg.getOperator();
    if (!(operator instanceof AggrFunction)) {
      throw new RepositoryException("AggrExpression的operator只能是" + AggrFunction.class);
    }

    List<Expression> expressions = agg.getExpressions();
    if (CollectionUtil.size(expressions) != 1) {
      throw new RepositoryException("AggrExpression的需要1个操作数");
    }
    return StrUtil.format(
        ((AggrFunction) operator)
            .toString(
                ExpressionHelper.toSql(
                    userContext, expressions.get(0), idTable, parameters, sqlColumnResolver)));
  }
}
