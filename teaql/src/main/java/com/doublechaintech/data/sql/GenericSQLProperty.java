package com.doublechaintech.data.sql;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.util.ReflectUtil;
import com.doublechaintech.data.BaseEntity;
import com.doublechaintech.data.Entity;
import com.doublechaintech.data.EntityStatus;
import com.doublechaintech.data.RepositoryException;
import com.doublechaintech.data.meta.PropertyDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

public class GenericSQLProperty extends PropertyDescriptor implements SQLProperty {
  private String tableName;
  private String columnName;
  private String columnType;

  public GenericSQLProperty(String pTableName, String pColumnName, String pType) {
    tableName = pTableName;
    columnName = pColumnName;
    columnType = pType;
  }

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
    if (value instanceof Entity) {
      d.setValue(((Entity) value).getId());
    } else {
      d.setValue(value);
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
    } else {
      Object value;
      try {
        value = rs.getObject(getName());
      } catch (SQLException pE) {
        throw new RepositoryException(pE);
      }
      entity.setProperty(getName(), Convert.convert(targetType, value));
    }
  }

  private boolean findName(ResultSet resultSet, String name) {
    try{
    int columnCount = resultSet.getMetaData().getColumnCount();
    for (int i = 0; i < columnCount; i++) {
      String columnLabel = resultSet.getMetaData().getColumnLabel(i + 1);
      if (columnLabel.equals(name)){
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
}
