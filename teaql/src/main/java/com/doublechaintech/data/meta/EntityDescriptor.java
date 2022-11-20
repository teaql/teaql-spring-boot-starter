package com.doublechaintech.data.meta;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.ObjectUtil;
import com.doublechaintech.data.Entity;
import com.doublechaintech.data.sql.GenericSQLProperty;
import com.doublechaintech.data.sql.GenericSQLRelation;

import java.util.ArrayList;
import java.util.List;

/**
 * Entity元信息定义
 *
 * @author jackytian
 */
public class EntityDescriptor {

  /** 元信息的简单名称 */
  private String type;

  /** 所包含的属性 */
  private List<PropertyDescriptor> properties = new ArrayList<>();

  /** 对应的java 对象的class */
  private Class<? extends Entity> targetType;

  /** 继承结构 */
  private EntityDescriptor parent;

  public PropertyDescriptor findProperty(String propertyName) {
    if (ObjectUtil.isEmpty(properties)) {
      return null;
    }
    return CollectionUtil.findOne(properties, p -> p.getName().equals(propertyName));
  }

  public List<PropertyDescriptor> getOwnProperties() {
    List<PropertyDescriptor> ret = new ArrayList<>();
    for (PropertyDescriptor property : properties) {
      if (!(property instanceof Relation)) {
        ret.add(property);
      } else if (((Relation) property).getRelationKeeper() == this) {
        ret.add(property);
      }
    }
    return ret;
  }

  public List<Relation> getOwnRelations() {
    List<Relation> ret = new ArrayList<>();
    for (PropertyDescriptor property : properties) {
      if (!(property instanceof Relation)) {
        continue;
      } else if (((Relation) property).getRelationKeeper() == this) {
        ret.add((Relation) property);
      }
    }
    return ret;
  }

  public List<Relation> getForeignRelations() {
    List<Relation> ret = new ArrayList<>();
    for (PropertyDescriptor property : properties) {
      if (!(property instanceof Relation)) {
        continue;
      } else if (((Relation) property).getRelationKeeper() != this) {
        ret.add((Relation) property);
      }
    }
    return ret;
  }

  public String getType() {
    return type;
  }

  public void setType(String pType) {
    type = pType;
  }

  public List<PropertyDescriptor> getProperties() {
    return properties;
  }

  public void setProperties(List<PropertyDescriptor> pProperties) {
    properties = pProperties;
  }

  public Class<? extends Entity> getTargetType() {
    return targetType;
  }

  public void setTargetType(Class<? extends Entity> pTargetType) {
    targetType = pTargetType;
  }

  public EntityDescriptor getParent() {
    return parent;
  }

  public void setParent(EntityDescriptor pParent) {
    parent = pParent;
  }

  public PropertyDescriptor findVersionProperty() {
    return getOwnProperties().stream().filter(p -> p.isVersion()).findFirst().orElse(null);
  }

  public PropertyDescriptor findIdProperty() {
    return getOwnProperties().stream().filter(p -> p.isId()).findFirst().orElse(null);
  }

  public EntityDescriptor addSimpleProperty(
      String propertyName, Class type, String tableName, String columnName, String columnType) {
    GenericSQLProperty property = new GenericSQLProperty(tableName, columnName, columnType);
    property.setName(propertyName);
    property.setType(new SimplePropertyType(type));
    property.setOwner(this);
    properties.add(property);
    return this;
  }

  public EntityDescriptor addObjectProperty(
      EntityMetaFactory factory,
      String propertyName,
      String parentType,
      String reverseName,
      Class<? extends Entity> parentClass,
      String tableName,
      String columnName,
      String columnType) {
    GenericSQLRelation relation = new GenericSQLRelation();
    relation.setOwner(this);
    relation.setName(propertyName);
    relation.setType(new SimplePropertyType(parentClass));
    relation.setRelationKeeper(this);
    relation.setTableName(tableName);
    relation.setColumnName(columnName);
    relation.setColumnType(columnType);
    properties.add(relation);

    // parent增加一个反向的关系
    EntityDescriptor refer = factory.resolveEntityDescriptor(parentType);
    Relation reverse = new Relation();
    reverse.setOwner(refer);
    reverse.setName(reverseName);
    reverse.setType(new SimplePropertyType(this.getTargetType()));
    reverse.setRelationKeeper(this);

    relation.setReverseProperty(reverse);
    reverse.setReverseProperty(relation);

    refer.properties.add(reverse);
    return this;
  }
}
