package io.teaql.data.hana;

import io.teaql.data.BaseEntity;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLConstraint;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;
import java.util.ArrayList;
import java.util.List;

public class HanaRepository<T extends BaseEntity> extends SQLRepository<T> {

  @Override
  protected void ensureIndexAndForeignKey(UserContext ctx) {}

  public HanaRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }
}
