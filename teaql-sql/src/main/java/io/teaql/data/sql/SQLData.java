package io.teaql.data.sql;

// SQLData 代表着存在数据库一行一列中的值，
// 在持久化Entity时，每个属性都被拆分为一组SQLData（save or update
public class SQLData {
  // 表名
  String tableName;

  // 列名
  String columnName;

  // 可以持久化的值
  Object value;

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

  public Object getValue() {
    return value;
  }

  public void setValue(Object pValue) {
    value = pValue;
  }
}
