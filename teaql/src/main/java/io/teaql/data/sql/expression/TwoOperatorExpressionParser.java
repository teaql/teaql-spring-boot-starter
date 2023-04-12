package io.teaql.data.sql.expression;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;
import io.teaql.data.Expression;
import io.teaql.data.PropertyFunction;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.Operator;
import io.teaql.data.criteria.TwoOperatorCriteria;
import io.teaql.data.sql.SQLColumnResolver;

import java.util.List;
import java.util.Map;

public class TwoOperatorExpressionParser implements SQLExpressionParser<TwoOperatorCriteria> {
  @Override
  public Class<TwoOperatorCriteria> type() {
    return TwoOperatorCriteria.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      TwoOperatorCriteria twoOperatorCriteria,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    List<Expression> expressions = twoOperatorCriteria.getExpressions();
    PropertyFunction operator = twoOperatorCriteria.getOperator();
    if (!(operator instanceof Operator)) {
      throw new RepositoryException("不支持的运算符:" + operator);
    }
    if (CollectionUtil.size(expressions) != 2) {
      throw new RepositoryException(operator + "运算符需要左右值");
    }
    Expression left = expressions.get(0);
    Expression right = expressions.get(1);
    String leftSQL =
        ExpressionHelper.toSql(userContext, left, idTable, parameters, sqlColumnResolver);
    String rightSQL =
        ExpressionHelper.toSql(userContext, right, idTable, parameters, sqlColumnResolver);
    return StrUtil.format("{} {} {}{}{}", leftSQL, getOp((Operator) operator), getPrefix((Operator) operator), rightSQL, getSuffix((Operator) operator));
  }

  private Object getSuffix(Operator operator) {
    switch (operator){
      case IN :
      case NOT_IN:
        return ")";
      case CONTAIN:
      case NOT_CONTAIN:
      case BEGIN_WITH:
      case NOT_BEGIN_WITH:
        return "%";
      default:
        return "";
    }
  }

  private Object getPrefix(Operator operator) {
   switch (operator){
     case IN :
     case NOT_IN:
       return "(";
     case CONTAIN:
     case NOT_CONTAIN:
     case END_WITH:
     case NOT_END_WITH:
       return "%";
     default:
       return "";
   }
  }

  private String getOp(Operator operator) {
    switch (operator) {
      case EQUAL:
        return "=";
      case NOT_EQUAL:
        return "<>";
      case CONTAIN:
      case BEGIN_WITH:
      case END_WITH:
        return "LIKE";
      case NOT_CONTAIN:
      case NOT_BEGIN_WITH:
      case NOT_END_WITH:
        return "NOT LIKE";
      case GREATER_THAN:
        return ">";
      case GREATER_THAN_OR_EQUAL:
        return ">=";
      case LESS_THAN:
        return "<";
      case LESS_THAN_OR_EQUAL:
        return "<=";
      case IN:
        return "IN";
      case NOT_IN:
        return "NOT IN";
      default:
        throw new RepositoryException("不支持的运算符:" + operator);
    }
  }
}
