package io.teaql.data.snowflake;

import io.teaql.data.Entity;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;
import java.sql.Connection;
import java.sql.SQLException;
import javax.sql.DataSource;

public class SnowflakeRepository<T extends Entity> extends SQLRepository<T> {
  public SnowflakeRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }

  @Override
  protected String findTableColumnsSql(DataSource dataSource, String table) {
    try (Connection connection = dataSource.getConnection()) {
      String databaseName = connection.getCatalog();
      String schemaName = connection.getSchema();
      return String.format(
          "select * from information_schema.columns where table_name = '%s' and table_schema = '%s' and table_catalog = '%s'",
          table.toUpperCase(), schemaName, databaseName);
    } catch (SQLException pE) {
      throw new RuntimeException(pE);
    }
  }
}
