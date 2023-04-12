package com.doublechaintech.data.meta;

/** Entity所包含的属性的元信息 */
public class PropertyDescriptor {

  /**
   * 这个属性所属，一个EntityDescriptor引用一组PropertyDescriptor
   * 每个PropertyDescriptor也会反过来引用EntityDescriptor（双向关联）
   */
  private EntityDescriptor owner;

  /** 该属性在其owner中的名称，在每个owner */
  private String name;

  /** 属性类型 */
  private PropertyType type;

  public PropertyDescriptor() {}

  public PropertyDescriptor(String pPropertyName, PropertyType pType) {
    this.setName(pPropertyName);
    this.setType(pType);
  }

  public EntityDescriptor getOwner() {
    return owner;
  }

  public void setOwner(EntityDescriptor pOwner) {
    owner = pOwner;
  }

  public String getName() {
    return name;
  }

  public void setName(String pName) {
    name = pName;
  }

  public PropertyType getType() {
    return type;
  }

  public void setType(PropertyType pType) {
    type = pType;
  }

  public boolean isId() {
    return getName().equals("id");
  }

  public boolean isVersion() {
    return getName().equals("version");
  }
}
