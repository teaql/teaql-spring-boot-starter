package io.teaql.data.hana;

import io.teaql.data.sql.GenericSQLProperty;
import io.teaql.data.sql.GenericSQLRelation;
import io.teaql.data.sql.SQLEntityDescriptor;

public class HanaEntityDescriptor extends SQLEntityDescriptor {
  @Override
  protected GenericSQLProperty createPropertyDescriptor(
      String tableName, String columnName, String columnType) {
    return new HanaProperty(tableName, columnName, columnType);
  }

  @Override
  protected GenericSQLRelation createRelationDescriptor(
      String tableName, String columnName, String columnType) {
    GenericSQLRelation relation = new HanaRelation();
    relation.setTableName(tableName);
    relation.setColumnName(columnName);
    relation.setColumnType(columnType);
    return relation;
  }
}
