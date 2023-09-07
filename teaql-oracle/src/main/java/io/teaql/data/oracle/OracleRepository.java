package io.teaql.data.oracle;

import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.util.StrUtil;
import io.teaql.data.Entity;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;
import java.time.LocalDateTime;
import java.time.LocalDate;
import javax.sql.DataSource;

public class OracleRepository<T extends Entity> extends SQLRepository<T> {
  public OracleRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }

  @Override
  protected String getSqlValue(Object value) {
    if (value instanceof LocalDateTime) {
      return StrUtil.format(
          "TO_TIMESTAMP('{}', 'yyyy-mm-dd hh:mi:ss')",
          LocalDateTimeUtil.formatNormal((LocalDateTime) value));
    }
    if (value instanceof LocalDate) {
      return StrUtil.format(
          "TO_TIMESTAMP('{}', 'yyyy-mm-dd hh:mi:ss')",
          LocalDateTimeUtil.formatNormal((LocalDateTime) value));
    }
    return super.getSqlValue(value);
  }
}
