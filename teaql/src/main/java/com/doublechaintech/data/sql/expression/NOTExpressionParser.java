package com.doublechaintech.data.sql.expression;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;
import com.doublechaintech.data.Expression;
import com.doublechaintech.data.SearchCriteria;
import com.doublechaintech.data.UserContext;
import com.doublechaintech.data.criteria.NOT;
import com.doublechaintech.data.sql.SQLColumnResolver;

import java.util.List;
import java.util.Map;

public class NOTExpressionParser implements SQLExpressionParser<NOT> {

  @Override
  public Class<NOT> type() {
    return NOT.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      NOT expression,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    List<Expression> expressions = expression.getExpressions();
    Expression sub = CollectionUtil.getFirst(expressions);
    if (sub == null) {
      return SearchCriteria.TRUE;
    }
    String subSql =
        ExpressionHelper.toSql(userContext, sub, idTable, parameters, sqlColumnResolver);
    if (SearchCriteria.TRUE.equalsIgnoreCase(subSql)) {
      return SearchCriteria.FALSE;
    }

    if (SearchCriteria.FALSE.equalsIgnoreCase(subSql)) {
      return SearchCriteria.TRUE;
    }
    return StrUtil.format("NOT ({})", subSql);
  }
}
