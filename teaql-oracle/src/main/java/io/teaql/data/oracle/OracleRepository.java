package io.teaql.data.oracle;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.map.CaseInsensitiveMap;
import cn.hutool.core.util.StrUtil;
import io.teaql.data.Entity;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLRepository;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
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
    return String.format(
        "SELECT TABLE_NAME, COLUMN_NAME, DATA_TYPE FROM USER_TAB_COLUMNS where TABLE_NAME=UPPER('%s')", table);
  }

  @Override
  protected void ensure(
          UserContext ctx, List<Map<String, Object>> tableInfo, String table, List<SQLColumn> columns) {
    List<Map<String, Object>> lowercaseTableInfo = new ArrayList<>();
    for (Map<String, Object> column : tableInfo) {
      Map<String, Object> lowercaseColumn = new HashMap<>();
      for (Map.Entry<String, Object> field: column.entrySet()) {
        lowercaseColumn.put(field.getKey().toLowerCase(), field.getValue().toString().toLowerCase());
      }
      lowercaseTableInfo.add(lowercaseColumn);
    }
    super.ensure(ctx, lowercaseTableInfo, table, columns);
  }
  @Override
  protected String calculateDBType(Map<String, Object> columnInfo) {
    String dataType = (String) columnInfo.get("data_type");
    switch (dataType) {
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
      case "int":
      case "integer":
        return "integer";
      case "decimal":
      case "numeric":
        return StrUtil.format(
                "numeric({},{})", columnInfo.get("numeric_precision"), columnInfo.get("numeric_scale"));
      case "text":
        return "text";
      case "time without time zone":
        return "time";
      case "timestamp":
      case "timestamp without time zone":
        return "timestamp";
      default:
        throw new RepositoryException("未处理的类型:" + dataType);
    }
  }
}
