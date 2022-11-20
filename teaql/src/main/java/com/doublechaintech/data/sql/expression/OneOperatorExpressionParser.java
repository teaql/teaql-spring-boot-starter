package com.doublechaintech.data.sql.expression;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;
import com.doublechaintech.data.Expression;
import com.doublechaintech.data.PropertyFunction;
import com.doublechaintech.data.RepositoryException;
import com.doublechaintech.data.UserContext;
import com.doublechaintech.data.criteria.OneOperatorCriteria;
import com.doublechaintech.data.criteria.Operator;
import com.doublechaintech.data.sql.SQLColumnResolver;

import java.util.List;
import java.util.Map;

public class OneOperatorExpressionParser implements SQLExpressionParser<OneOperatorCriteria> {
  @Override
  public Class<OneOperatorCriteria> type() {
    return OneOperatorCriteria.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      OneOperatorCriteria criteria,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    List<Expression> expressions = criteria.getExpressions();
    PropertyFunction operator = criteria.getOperator();
    if (!(operator instanceof Operator)) {
      throw new RepositoryException("不支持的运算符:" + operator);
    }
    if (CollectionUtil.size(expressions) != 1) {
      throw new RepositoryException(operator + "运算符只能有左值");
    }
    Expression left = expressions.get(0);
    String leftSQL =
        ExpressionHelper.toSql(userContext, left, idTable, parameters, sqlColumnResolver);
    return StrUtil.format("{} {}", leftSQL, getOp((Operator) operator));
  }

  private Object getOp(Operator operator) {
    switch (operator) {
      case IS_NULL:
        return "IS NULL";
      case IS_NOT_NULL:
        return "IS NOT NULL";
      default:
        throw new RepositoryException("不支持的运算符:" + operator);
    }
  }
}
