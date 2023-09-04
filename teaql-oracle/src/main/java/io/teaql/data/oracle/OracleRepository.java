package io.teaql.data.oracle;

import io.teaql.data.Entity;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;

public class OracleRepository<T extends Entity> extends SQLRepository<T> {
  public OracleRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }
}
