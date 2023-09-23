package io.teaql.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import io.teaql.data.Parameter;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.Operator;
import io.teaql.data.sql.SQLRepository;
import java.util.List;
import java.util.Map;

public class ParameterParser implements SQLExpressionParser<Parameter> {
  @Override
  public Class<Parameter> type() {
    return Parameter.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      Parameter parameter,
      String pIdTable,
      Map<String, Object> parameters,
      SQLRepository sqlColumnResolver) {
    String key = nextPropertyKey(parameters, parameter.getName());
    Operator operator = parameter.getOperator();
    Object value = parameter.getValue();
    if (operator != null) {
      value = fixValue(operator, parameter.getValue());
    }
    parameters.put(key, value);
    return StrUtil.format(":{}", key);
  }

  public Object fixValue(Operator pOperator, Object pValue) {
    switch (pOperator) {
      case CONTAIN:
      case NOT_CONTAIN:
        return "%" + pValue + "%";
      case BEGIN_WITH:
      case NOT_BEGIN_WITH:
        return pValue + "%";
      case END_WITH:
      case NOT_END_WITH:
        return "%" + pValue;
      case IN_LARGE:
      case NOT_IN_LARGE:
        List flatValues = Parameter.flatValues(pValue);
        return flatValues.toArray(new Object[flatValues.size()]);
    }
    return pValue;
  }
}
