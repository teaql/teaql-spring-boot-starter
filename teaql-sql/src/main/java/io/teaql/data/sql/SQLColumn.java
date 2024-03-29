package io.teaql.data.sql;

public class SQLColumn {
  String tableName;
  String columnName;
  String type;

  public SQLColumn(String pTableName, String pColumnName) {
    tableName = pTableName;
    columnName = pColumnName;
  }

  public String getTableName() {
    return tableName;
  }

  public void setTableName(String pTableName) {
    tableName = pTableName;
  }

  public String getColumnName() {
    return columnName;
  }

  public void setColumnName(String pColumnName) {
    columnName = pColumnName;
  }

  public String getType() {
    return type;
  }

  public void setType(String pType) {
    type = pType;
  }
}
