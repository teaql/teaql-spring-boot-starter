package io.teaql.data.sql.expression;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;
import io.teaql.data.*;
import io.teaql.data.sql.SQLRepository;
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
      SQLRepository sqlColumnResolver) {
    PropertyFunction operator = agg.getOperator();
    if (!(operator instanceof AggrFunction)) {
      throw new RepositoryException("AggrExpression的operator只能是" + AggrFunction.class);
    }

    List<Expression> expressions = agg.getExpressions();
    if (CollectionUtil.size(expressions) != 1) {
      throw new RepositoryException("AggrExpression的需要1个操作数");
    }
    String sqlColumn =
        ExpressionHelper.toSql(
            userContext, expressions.get(0), idTable, parameters, sqlColumnResolver);
    AggrFunction aggrFunction = (AggrFunction) operator;
    switch (aggrFunction) {
      case SELF:
        return sqlColumn;
      case MIN:
        return StrUtil.format("min({})", sqlColumn);
      case MAX:
        return StrUtil.format("max({})", sqlColumn);
      case SUM:
        return StrUtil.format("sum({})", sqlColumn);
      case COUNT:
        return StrUtil.format("count({})", sqlColumn);
      case GBK:
        return StrUtil.format("convert_to({},'GBK')", sqlColumn);
    }
    throw new RepositoryException("不支持的聚合函数:" + aggrFunction);
  }
}
