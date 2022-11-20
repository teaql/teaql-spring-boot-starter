package com.doublechaintech.data.sql;

import com.doublechaintech.data.Entity;

import java.sql.ResultSet;
import java.util.List;

public interface SQLProperty {

  List<SQLColumn> columns();

  List<SQLData> toDBRaw(Object value);

  void setPropertyValue(Entity entity, ResultSet rs);
}
