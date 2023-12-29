package io.teaql.data.hana;

import cn.hutool.core.util.StrUtil;
import io.teaql.data.BaseEntity;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLConstraint;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class HanaRepository<T extends BaseEntity> extends SQLRepository<T> {

  @Override
  protected void ensureIndexAndForeignKey(UserContext ctx) {
  }

  public HanaRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }

  @Override
  protected String findTableColumnsSql(DataSource dataSource, String table) {
    try (Connection connection = dataSource.getConnection()) {
      String databaseName = connection.getCatalog();
      String schemaName = connection.getSchema();
      return String.format(
              "select * from table_columns where table_name = '%s' and schema_name = '%s'",
              table.toUpperCase(), schemaName);
    } catch (SQLException pE) {
      throw new RuntimeException(pE);
    }

  }

  @Override
  protected String getPureColumnName(String columnName) {
    if (!columnName.startsWith("\"")) {
      columnName = columnName.toUpperCase();
    }
    return StrUtil.unWrap(columnName, '\"');
  }

  @Override

  protected String calculateDBType(Map<String, Object> columnInfo) {
    String dataType = ((String) columnInfo.get("DATA_TYPE_NAME")).toLowerCase();
    switch (dataType) {
      case "bigint":
        return "bigint";
      case "tinyint":
      case "boolean":
        return "boolean";
      case "varchar":
      case "character varying":
        return StrUtil.format("varchar({})", columnInfo.get("LENGTH"));
      case "date":
        return "date";
      case "int":
      case "integer":
        return "integer";
      case "decimal":
      case "numeric":
        return StrUtil.format(
                "numeric({},{})", columnInfo.get("LENGTH"), columnInfo.get("SCALE"));
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