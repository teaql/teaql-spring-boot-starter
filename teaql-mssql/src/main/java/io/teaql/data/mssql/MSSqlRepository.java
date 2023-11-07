package io.teaql.data.mssql;

import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import io.teaql.data.Entity;
import io.teaql.data.RepositoryException;
import io.teaql.data.SearchRequest;
import io.teaql.data.Slice;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Map;

public class MSSqlRepository<T extends Entity> extends SQLRepository<T> {
  public MSSqlRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }

  @Override
  protected String getSqlValue(Object value) {
    if (value instanceof LocalDateTime) {
      return StrUtil.format("'{}'",LocalDateTimeUtil.formatNormal((LocalDateTime) value));
    }
    if (value instanceof LocalDate) {
      return StrUtil.format("'{}'",LocalDateTimeUtil.formatNormal((LocalDate) value));
    }
    return super.getSqlValue(value);
  }

  @Override
  protected String calculateDBType(Map<String, Object> columnInfo) {
    String dataType = (String) columnInfo.get("data_type");
    switch (dataType) {
      case "number":
        if ("0".equals(columnInfo.get("data_scale"))) {
          return StrUtil.format(
                  "number({})", columnInfo.get("data_precision"));
        }
        return StrUtil.format(
                "number({},{})", columnInfo.get("data_precision"), columnInfo.get("data_scale"));
      case "varchar2":
        return StrUtil.format("varchar({})", columnInfo.get("data_length"));
      case "bigint":
        return "bigint";
      case "tinyint":
      case "boolean":
        return "boolean";
      case "varchar":
      case "character varying":
        return StrUtil.format("varchar({})", columnInfo.get("character_maximum_length"));
      case "date":
        return "date";
      case "datetime":
        return "datetime";
      case "int":
      case "integer":
        return "integer";
      case "decimal":
      case "numeric":
        return StrUtil.format(
                "numeric({},{})", columnInfo.get("numeric_precision"), columnInfo.get("numeric_scale"));
      case "text":
        return "text";
      case "clob":
        return "clob";
      case "time without time zone":
        return "time";
      case "timestamp":
      case "timestamp(6)":
      case "timestamp without time zone":
        return "timestamp";
      default:
        throw new RepositoryException("未处理的类型:" + dataType);
    }
  }

  @Override
  protected String prepareLimit(SearchRequest request) {
    Slice slice = request.getSlice();
    if (ObjectUtil.isEmpty(slice)) {
      return null;
    }
//    request.getOrderBy().isEmpty()
    return StrUtil.format("OFFSET {} ROWS FETCH NEXT {} ROWS ONLY", slice.getOffset(), slice.getSize());
  }
}
