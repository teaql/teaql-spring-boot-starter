package io.teaql.data.mssql;

import io.teaql.data.Entity;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;

public class MSSqlRepository<T extends Entity> extends SQLRepository<T> {
  public MSSqlRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }
}
