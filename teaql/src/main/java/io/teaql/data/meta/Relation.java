package io.teaql.data.meta;

/** 关系作为一个特殊的属性类型 */
public class Relation extends PropertyDescriptor {

  /** 关系是双向的，用此来表示反向关系（即从另一个EntityDescriptor的一个关系属性来表示） */
  private PropertyDescriptor reverseProperty;

  /** 关系由某一个entity来维护 */
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
