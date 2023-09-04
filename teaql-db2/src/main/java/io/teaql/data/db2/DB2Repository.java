package io.teaql.data.db2;

import io.teaql.data.BaseEntity;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;

public class DB2Repository<T extends BaseEntity> extends SQLRepository<T> {
  public DB2Repository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }
}
