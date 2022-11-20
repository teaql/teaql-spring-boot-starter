package com.doublechaintech.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import com.doublechaintech.data.Parameter;
import com.doublechaintech.data.UserContext;
import com.doublechaintech.data.sql.SQLColumnResolver;

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
      SQLColumnResolver sqlColumnResolver) {
    String key = nextPropertyKey(parameters, parameter.getName());
    parameters.put(key, parameter.getValue());
    return StrUtil.format(":{}", key);
  }
}
