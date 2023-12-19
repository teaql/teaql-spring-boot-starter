package io.teaql.data.db2;

import io.teaql.data.BaseEntity;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.sql.SQLConstraint;
import io.teaql.data.sql.SQLRepository;
import javax.sql.DataSource;
import java.util.ArrayList;
import java.util.List;

public class DB2Repository<T extends BaseEntity> extends SQLRepository<T> {
  @Override
  protected List<SQLConstraint> fetchFKs(UserContext ctx) {
    return new ArrayList<>();
  }
  public DB2Repository(EntityDescriptor entityDescriptor, DataSource dataSource) {
    super(entityDescriptor, dataSource);
  }
}
