package io.teaql.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import io.teaql.data.PropertyReference;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLColumnResolver;

import java.util.Map;

public class PropertyParser implements SQLExpressionParser<PropertyReference> {

  @Override
  public Class<PropertyReference> type() {
    return PropertyReference.class;
  }

  @Override
  public String toSql(
      UserContext userContext,
      PropertyReference property,
      String idTable,
      Map<String, Object> parameters,
      SQLColumnResolver sqlColumnResolver) {
    String propertyName = property.getPropertyName();
    SQLColumn propertyColumn = sqlColumnResolver.getPropertyColumn(idTable, propertyName);
    return StrUtil.format("{}.{}", propertyColumn.getTableName(), propertyColumn.getColumnName());
  }
}
