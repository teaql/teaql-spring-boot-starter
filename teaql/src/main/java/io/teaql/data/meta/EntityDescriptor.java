package io.teaql.data.meta;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.BooleanUtil;
import cn.hutool.core.util.ObjectUtil;
import io.teaql.data.Entity;
import java.util.*;

/**
 * Entity metadata
 *
 * @author jackytian
 */
public class EntityDescriptor {

  /** entity type name */
  private String type;

  /** the properties */
  private List<PropertyDescriptor> properties = new ArrayList<>();

  /** java type */
  private Class<? extends Entity> targetType;

  /** parent entity descriptor */
  private EntityDescriptor parent;

  private Set<EntityDescriptor> children = new HashSet<>();

  private Map<String, String> additionalInfo = new HashMap<>();

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
    if (parent != null) {
      parent.addChild(this);
    }
  }

  private void addChild(EntityDescriptor child) {
    this.children.add(child);
  }

  public Set<EntityDescriptor> getChildren() {
    return children;
  }

  public boolean hasChildren() {
    return !children.isEmpty();
  }

  public PropertyDescriptor findVersionProperty() {
    return getOwnProperties().stream().filter(p -> p.isVersion()).findFirst().orElse(null);
  }

  public PropertyDescriptor findIdProperty() {
    return getOwnProperties().stream().filter(p -> p.isId()).findFirst().orElse(null);
  }

  @Override
  public boolean equals(Object pO) {
    if (this == pO) return true;
    if (pO == null || getClass() != pO.getClass()) return false;
    EntityDescriptor that = (EntityDescriptor) pO;
    return getType().equals(that.getType());
  }

  @Override
  public int hashCode() {
    return Objects.hash(getType());
  }

  public boolean isRoot() {
    return getParent() == null && getOwnRelations().isEmpty();
  }

  public EntityDescriptor with(String key, String value) {
    additionalInfo.put(key, value);
    return this;
  }

  public Map<String, String> getAdditionalInfo() {
    return additionalInfo;
  }

  public void setAdditionalInfo(Map<String, String> pAdditionalInfo) {
    additionalInfo = pAdditionalInfo;
  }

  public boolean isConstant() {
    String constant = getAdditionalInfo().get("constant");
    return BooleanUtil.toBoolean(constant);
  }

  public PropertyDescriptor getIdentifier() {
    for (PropertyDescriptor ownProperty : getOwnProperties()) {
      if (ownProperty.isIdentifier()) {
        return ownProperty;
      }
    }
    return null;
  }

  public PropertyDescriptor addSimpleProperty(String propertyName, Class type) {
    PropertyDescriptor property = createPropertyDescriptor();
    property.setName(propertyName);
    property.setType(new SimplePropertyType(type));
    property.setOwner(this);
    getProperties().add(property);
    return property;
  }

  protected PropertyDescriptor createPropertyDescriptor() {
    return new PropertyDescriptor();
  }

  public Relation addObjectProperty(
      EntityMetaFactory factory,
      String propertyName,
      String parentType,
      String reverseName,
      Class<? extends Entity> parentClass) {
    Relation relation = createRelation();
    relation.setOwner(this);
    relation.setName(propertyName);
    relation.setType(new SimplePropertyType(parentClass));
    relation.setRelationKeeper(this);
    getProperties().add(relation);

    // add one reverse relation on parent
    EntityDescriptor refer = factory.resolveEntityDescriptor(parentType);
    Relation reverse = new Relation();
    reverse.setOwner(refer);
    reverse.setName(reverseName);
    reverse.setType(new SimplePropertyType(this.getTargetType()));
    reverse.setRelationKeeper(this);

    relation.setReverseProperty(reverse);
    reverse.setReverseProperty(relation);

    refer.getProperties().add(reverse);
    return relation;
  }

  protected Relation createRelation() {
    return new Relation();
  }
}
