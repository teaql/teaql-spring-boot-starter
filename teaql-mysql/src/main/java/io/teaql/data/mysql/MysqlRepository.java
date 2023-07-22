package io.teaql.data.mysql;

import io.teaql.data.Entity;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;

public class MysqlRepository<T extends Entity> extends SQLRepository<T> {
  public MysqlRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }
}
