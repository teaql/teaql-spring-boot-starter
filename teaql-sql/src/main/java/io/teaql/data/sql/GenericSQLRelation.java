package io.teaql.data.sql;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.util.ReflectUtil;
import io.teaql.data.BaseEntity;
import io.teaql.data.Entity;
import io.teaql.data.EntityStatus;
import io.teaql.data.RepositoryException;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.Relation;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

public class GenericSQLRelation extends Relation implements SQLProperty {
  private String tableName;
  private String columnName;
  private String columnType;

  @Override
  public List<SQLColumn> columns() {
    SQLColumn sqlColumn = new SQLColumn(tableName, columnName);
    sqlColumn.setType(columnType);
    return ListUtil.of(sqlColumn);
  }

  @Override
  public List<SQLData> toDBRaw(Object value) {
    SQLData d = new SQLData();
    d.setColumnName(columnName);
    d.setTableName(tableName);
    if (value == null) {
      d.setValue(null);
    } else if (value instanceof Entity) {
      d.setValue(((Entity) value).getId());
    } else {
      throw new RepositoryException("Relation only support Entity class");
    }
    return ListUtil.of(d);
  }

  @Override
  public void setPropertyValue(Entity entity, ResultSet rs) {
    if (!findName(rs, getName())){
      return;
    }
    Class targetType = getType().javaType();
    if (Entity.class.isAssignableFrom(targetType)) {
      Entity o = createRefer(this, rs, targetType);
      entity.setProperty(getName(), o);
      return;
    }
    throw new RepositoryException("Relation only support Entity class");
  }

  private boolean findName(ResultSet resultSet, String name) {
    try{
      int columnCount = resultSet.getMetaData().getColumnCount();
      for (int i = 0; i < columnCount; i++) {
        String columnLabel = resultSet.getMetaData().getColumnLabel(i + 1);
        if (columnLabel.equalsIgnoreCase(name)){
          return true;
        }
      }
    }catch (Exception e){

    }
    return false;
  }

  private Entity createRefer(PropertyDescriptor pProperty, ResultSet resultSet, Class targetType) {
    BaseEntity o = (BaseEntity) ReflectUtil.newInstance(targetType);
    Object referId;
    try {
      referId = resultSet.getObject(pProperty.getName());
    } catch (SQLException pE) {
      throw new RepositoryException(pE);
    }
    if (referId == null) {
      return null;
    }
    o.setId(((Number) referId).longValue());
    o.set$status(EntityStatus.REFER);
    return o;
  }

  public void setTableName(String pTableName) {
    tableName = pTableName;
  }

  public void setColumnName(String pColumnName) {
    columnName = pColumnName;
  }

  public void setColumnType(String pColumnType) {
    columnType = pColumnType;
  }

  public String getTableName() {
    return tableName;
  }

  public String getColumnName() {
    return columnName;
  }

  public String getColumnType() {
    return columnType;
  }
}
