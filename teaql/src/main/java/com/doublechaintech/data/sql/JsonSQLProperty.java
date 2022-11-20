package com.doublechaintech.data.sql;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.text.NamingCase;
import cn.hutool.json.JSONUtil;
import com.doublechaintech.data.Entity;
import com.doublechaintech.data.RepositoryException;
import com.doublechaintech.data.meta.PropertyDescriptor;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

public class JsonSQLProperty extends PropertyDescriptor implements SQLProperty {
  @Override
  public List<SQLColumn> columns() {
    return ListUtil.of(
        new SQLColumn(
            NamingCase.toUnderlineCase(getName()),
            NamingCase.toUnderlineCase(getOwner().getType())));
  }

  @Override
  public List<SQLData> toDBRaw(Object v) {
    SQLData d = new SQLData();
    d.setColumnName(NamingCase.toUnderlineCase(getName()));
    d.setTableName(NamingCase.toUnderlineCase(getOwner().getType()));
    d.setValue(JSONUtil.toJsonStr(v));
    return ListUtil.of(d);
  }

  @Override
  public void setPropertyValue(Entity entity, ResultSet rs) {
    try {
      String json = rs.getString(NamingCase.toUnderlineCase(getName()));
      Object o = JSONUtil.toBean(json, getType().javaType());
      entity.setProperty(getName(), o);
    } catch (SQLException pE) {
      throw new RepositoryException(pE);
    }
  }
}
