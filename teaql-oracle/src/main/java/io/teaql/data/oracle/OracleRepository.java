package io.teaql.data.oracle;

import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.util.StrUtil;
import io.teaql.data.Entity;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
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
          "TO_DATE('{}', 'yyyy-mm-dd')", LocalDateTimeUtil.formatNormal((LocalDate) value));
    }
    return super.getSqlValue(value);
  }

  public String getIdSpaceSql() {
    StringBuilder sb = new StringBuilder();
    sb.append("CREATE TABLE ")
        .append(getTqlIdSpaceTable())
        .append(" (\n")
        .append("type_name varchar(100) PRIMARY KEY,\n")
        .append("current_level number(11))\n");
    String createIdSpaceSql = sb.toString();
    return createIdSpaceSql;
  }

  @Override
  protected String findTableColumnsSql(DataSource dataSource, String table) {
    try {
      return String.format(
          "SELECT * FROM ALL_TABLES WHERE OWNER=UPPER('%s') AND TABLE_NAME=UPPER('%s')",
          dataSource.getConnection().getSchema(), table);
    } catch (SQLException e) {
      throw new RuntimeException(e);
    }
  }
}
