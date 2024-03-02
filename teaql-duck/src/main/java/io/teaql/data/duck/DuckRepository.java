package io.teaql.data.duck;

import io.teaql.data.Entity;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;

public class DuckRepository<T extends Entity> extends SQLRepository<T> {
  public DuckRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }
}
