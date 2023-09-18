package io.teaql.data.oracle;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.db.DbUtil;
import io.teaql.data.Entity;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;

import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import javax.sql.DataSource;

public class OracleRepository<T extends Entity> extends SQLRepository<T> {
  public OracleRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }

  @Override
  protected String getSqlValue(Object value) {
    if (value instanceof LocalDateTime) {
      return StrUtil.format(
          "TO_TIMESTAMP('{}', 'yyyy-mm-dd hh24:mi:ss')",
          LocalDateTimeUtil.formatNormal((LocalDateTime) value));
    }
    if (value instanceof LocalDate) {
      return StrUtil.format(
          "TO_DATE('{}', 'yyyy-mm-dd')",
          LocalDateTimeUtil.formatNormal((LocalDate) value));
    }
    return super.getSqlValue(value);
  }
  @Override
  protected void ensureIdSpaceTable(UserContext ctx) {
    String sql = findIdSpaceTableSql();
    List<Map<String, Object>> dbTableInfo;
    try {
      dbTableInfo = DbUtil.use(getDataSource()).query(sql, mapList());
    } catch (Exception exception) {
      dbTableInfo = ListUtil.empty();
    }

    if (!ObjectUtil.isEmpty(dbTableInfo)) {
      return;
    }

    StringBuilder sb = new StringBuilder();
    sb.append("CREATE TABLE ")
            .append(getTqlIdSpaceTable())
            .append(" (\n")
            .append("type_name varchar(100) PRIMARY KEY,\n")
            .append("current_level number(11))\n");
    String createIdSpaceSql = sb.toString();
    ctx.info(createIdSpaceSql + ";");
    if (ctx.config() != null && ctx.config().isEnsureTable()) {
      try {
        DbUtil.use(getDataSource()).execute(createIdSpaceSql);
      } catch (SQLException pE) {
        throw new RepositoryException(pE);
      }
    }
  }

  @Override
  protected String findTableColumnsSql(DataSource dataSource, String table) {
    try {
      return String.format("SELECT * FROM ALL_TABLES WHERE OWNER=UPPER('%s') AND TABLE_NAME=UPPER('%s')", dataSource.getConnection().getSchema(), table);
    } catch (SQLException e) {
      throw new RuntimeException(e);
    }
  }
}
