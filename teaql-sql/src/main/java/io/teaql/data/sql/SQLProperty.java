package io.teaql.data.sql;

import io.teaql.data.Entity;
import io.teaql.data.UserContext;
import java.sql.ResultSet;
import java.util.List;

public interface SQLProperty {

  List<SQLColumn> columns();

  List<SQLData> toDBRaw(UserContext ctx, Entity entity, Object value);

  void setPropertyValue(UserContext ctx, Entity entity, ResultSet rs);
}
