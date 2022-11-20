package com.doublechaintech.data.sql.expression;

import cn.hutool.core.util.StrUtil;
import com.doublechaintech.data.PropertyReference;
import com.doublechaintech.data.UserContext;
import com.doublechaintech.data.sql.SQLColumn;
import com.doublechaintech.data.sql.SQLColumnResolver;

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
