package io.teaql.data;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.util.ObjectUtil;
import java.util.ArrayList;
import java.util.List;

public class FunctionApply implements Expression {
  PropertyFunction operator;
  List<Expression> expressions;

  public FunctionApply(PropertyFunction operator, Expression... expressions) {
    if (ObjectUtil.isEmpty(expressions)) {
      throw new RepositoryException("FunctionApply的expressions不能为空");
    }
    this.operator = operator;
    this.expressions = new ArrayList<>(ListUtil.of(expressions));
  }

  @Override
  public List<String> properties(UserContext ctx) {
    List<String> ret = new ArrayList<>();

    for (Expression expression : expressions) {
      List<String> properties = expression.properties(ctx);
      if (properties != null) {
        ret.addAll(properties);
      }
    }
    return ret;
  }

  public PropertyFunction getOperator() {
    return operator;
  }

  public List<Expression> getExpressions() {
    return expressions;
  }

  public Expression first() {
    return CollUtil.getFirst(expressions);
  }

  public Expression second() {
    return CollUtil.get(expressions, 1);
  }

  public Expression third() {
    return CollUtil.get(expressions, 2);
  }

  public Expression last() {
    return CollUtil.get(expressions, -1);
  }
}
