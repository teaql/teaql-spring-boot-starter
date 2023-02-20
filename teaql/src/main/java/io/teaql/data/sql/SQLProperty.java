package io.teaql.data.sql;

import io.teaql.data.Entity;

import java.sql.ResultSet;
import java.util.List;

public interface SQLProperty {

  List<SQLColumn> columns();

  List<SQLData> toDBRaw(Object value);

  void setPropertyValue(Entity entity, ResultSet rs);
}
