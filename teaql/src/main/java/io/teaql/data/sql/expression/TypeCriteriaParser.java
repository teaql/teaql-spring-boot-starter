package io.teaql.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import io.teaql.data.Parameter;
import io.teaql.data.TypeCriteria;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLColumnResolver;

import java.util.Map;

public class TypeCriteriaParser implements SQLExpressionParser<TypeCriteria> {
  @Override
  public Class<TypeCriteria> type() {
    return SQLExpressionParser.super.type();
  }

  @Override
  public String toSql(
      UserContext userContext,
      TypeCriteria expression,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    Parameter typeParameter = expression.getTypeParameter();
    String parameterSql =
        ExpressionHelper.toSql(userContext, typeParameter, idTable, parameters, sqlColumnResolver);
    SQLColumn childType = sqlColumnResolver.getPropertyColumn(idTable, "_child_type");
    return StrUtil.format("{}._child_type in ({})", childType.getTableName(), parameterSql);
  }
}
