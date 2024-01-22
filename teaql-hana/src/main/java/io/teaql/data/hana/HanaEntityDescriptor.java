package io.teaql.data.hana;

import io.teaql.data.sql.GenericSQLProperty;
import io.teaql.data.sql.GenericSQLRelation;
import io.teaql.data.sql.SQLEntityDescriptor;

public class HanaEntityDescriptor extends SQLEntityDescriptor {
  @Override
  protected GenericSQLProperty createPropertyDescriptor() {
    return new HanaProperty();
  }

  @Override
  protected GenericSQLRelation createRelation() {
    return new HanaRelation();
  }
}
