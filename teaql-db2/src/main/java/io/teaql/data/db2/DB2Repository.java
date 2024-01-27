package io.teaql.data.db2;

import io.teaql.data.BaseEntity;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;

public class DB2Repository<T extends BaseEntity> extends SQLRepository<T> {
  public DB2Repository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }

  protected String wrapColumnStatementForCreatingTable(UserContext ctx, String table, SQLColumn column){

    String dbColumn = column.getColumnName() + " " + column.getType();
    if ("id".equalsIgnoreCase(column.getColumnName())) {
      dbColumn = dbColumn + " PRIMARY KEY NOT NULL";
    }
    return dbColumn;
  }

  public String getIdSpaceSql() {
    StringBuilder sb = new StringBuilder();
    sb.append("CREATE TABLE ")
            .append(getTqlIdSpaceTable())
            .append(" (\n")
            .append("type_name varchar(100) PRIMARY KEY NOT NULL,\n")
            .append("current_level bigint)\n");
    String createIdSpaceSql = sb.toString();
    return createIdSpaceSql;
  }

  @Override
  protected void ensureIndexAndForeignKey(UserContext ctx) {}
}
