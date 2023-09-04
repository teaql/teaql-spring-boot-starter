package io.teaql.data.hana;

import io.teaql.data.BaseEntity;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;

public class HanaRepository<T extends BaseEntity> extends SQLRepository<T> {

  public HanaRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }
}
