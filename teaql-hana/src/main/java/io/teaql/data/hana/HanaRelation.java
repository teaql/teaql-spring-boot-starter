package io.teaql.data.hana;

import io.teaql.data.sql.GenericSQLRelation;
import java.sql.ResultSet;

public class HanaRelation extends GenericSQLRelation {
  @Override
  protected boolean findName(ResultSet resultSet, String name) {
    return super.findName(resultSet, name);
  }

  @Override
  protected Object getValue(ResultSet resultSet) {
    return super.getValue(resultSet);
  }
}
