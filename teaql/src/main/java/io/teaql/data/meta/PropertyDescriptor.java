package io.teaql.data.meta;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.util.BooleanUtil;
import cn.hutool.core.util.StrUtil;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

  private Map<String, String> additionalInfo = new HashMap<>();

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

  public PropertyDescriptor with(String key, String value) {
    additionalInfo.put(key, value);
    return this;
  }

  public Map<String, String> getAdditionalInfo() {
    return additionalInfo;
  }

  public void setAdditionalInfo(Map<String, String> pAdditionalInfo) {
    additionalInfo = pAdditionalInfo;
  }

  public boolean isIdentifier() {
    String identifier = getAdditionalInfo().get("identifier");
    return BooleanUtil.toBoolean(identifier);
  }

  public List<String> getCandidates() {
    String candidates = getAdditionalInfo().get("candidates");
    if (candidates == null) {
      return ListUtil.empty();
    }
    return StrUtil.split(candidates, ",", true, true);
  }
}
