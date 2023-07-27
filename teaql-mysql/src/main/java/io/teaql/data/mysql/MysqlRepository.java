package io.teaql.data.mysql;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.map.CaseInsensitiveMap;
import io.teaql.data.Entity;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;
import java.util.List;
import java.util.Map;

public class MysqlRepository<T extends Entity> extends SQLRepository<T> {
  public MysqlRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }


  @Override
  protected void ensure(UserContext ctx, List<Map<String, Object>> tableInfo, String table, List<SQLColumn> columns) {
    tableInfo = CollStreamUtil.toList(tableInfo, CaseInsensitiveMap::new);
    super.ensure(ctx, tableInfo, table, columns);
  }
}
