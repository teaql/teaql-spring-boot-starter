package io.teaql.data.meta;

/** special property */
public class Relation extends PropertyDescriptor {

  /** reverse property */
  private PropertyDescriptor reverseProperty;

  /** the relation keeper */
  private EntityDescriptor relationKeeper;

  public PropertyDescriptor getReverseProperty() {
    return reverseProperty;
  }

  public void setReverseProperty(PropertyDescriptor pReverseProperty) {
    reverseProperty = pReverseProperty;
  }

  public EntityDescriptor getRelationKeeper() {
    return relationKeeper;
  }

  public void setRelationKeeper(EntityDescriptor pRelationKeeper) {
    relationKeeper = pRelationKeeper;
  }
}
