package io.teaql.data.hana;

import io.teaql.data.sql.GenericSQLProperty;
import java.sql.ResultSet;

public class HanaProperty extends GenericSQLProperty {
  public HanaProperty(String pTableName, String pColumnName, String pType) {
    super(pTableName, pColumnName, pType);
  }

  @Override
  protected boolean findName(ResultSet resultSet, String name) {
    return super.findName(resultSet, name);
  }

  @Override
  protected Object getValue(ResultSet rs) {
    return super.getValue(rs);
  }
}
