package io.teaql.data.sql;

import io.teaql.data.Entity;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.EntityMetaFactory;
import io.teaql.data.meta.Relation;
import io.teaql.data.meta.SimplePropertyType;

public class SQLEntityDescriptor extends EntityDescriptor {
  public SQLEntityDescriptor addSimpleProperty(
      String propertyName, Class type, String tableName, String columnName, String columnType) {
    GenericSQLProperty property = createPropertyDescriptor(tableName, columnName, columnType);
    property.setName(propertyName);
    property.setType(new SimplePropertyType(type));
    property.setOwner(this);
    getProperties().add(property);
    return this;
  }

  protected GenericSQLProperty createPropertyDescriptor(
      String tableName, String columnName, String columnType) {
    return new GenericSQLProperty(tableName, columnName, columnType);
  }

  public SQLEntityDescriptor addObjectProperty(
      EntityMetaFactory factory,
      String propertyName,
      String parentType,
      String reverseName,
      Class<? extends Entity> parentClass,
      String tableName,
      String columnName,
      String columnType) {
    GenericSQLRelation relation = createRelationDescriptor(tableName, columnName, columnType);
    relation.setOwner(this);
    relation.setName(propertyName);
    relation.setType(new SimplePropertyType(parentClass));
    relation.setRelationKeeper(this);
    getProperties().add(relation);

    //add one reverse relation on parent
    EntityDescriptor refer = factory.resolveEntityDescriptor(parentType);
    Relation reverse = new Relation();
    reverse.setOwner(refer);
    reverse.setName(reverseName);
    reverse.setType(new SimplePropertyType(this.getTargetType()));
    reverse.setRelationKeeper(this);

    relation.setReverseProperty(reverse);
    reverse.setReverseProperty(relation);

    refer.getProperties().add(reverse);
    return this;
  }

  protected GenericSQLRelation createRelationDescriptor(
      String tableName, String columnName, String columnType) {
    GenericSQLRelation relation = new GenericSQLRelation();
    relation.setTableName(tableName);
    relation.setColumnName(columnName);
    relation.setColumnType(columnType);
    return relation;
  }
}
