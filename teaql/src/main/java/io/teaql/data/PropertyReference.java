package io.teaql.data;

import cn.hutool.core.collection.ListUtil;
import java.util.List;
import java.util.Objects;

public class PropertyReference implements Expression, PropertyAware {
  String propertyName;

  public PropertyReference(String propertyName) {
    this.propertyName = propertyName;
  }

  public String getPropertyName() {
    return propertyName;
  }

  public void setPropertyName(String pPropertyName) {
    propertyName = pPropertyName;
  }

  @Override
  public List<String> properties(UserContext ctx) {
    return ListUtil.of(this.propertyName);
  }

  @Override
  public boolean equals(Object pO) {
    if (this == pO) return true;
    if (!(pO instanceof PropertyReference that)) return false;
    return Objects.equals(getPropertyName(), that.getPropertyName());
  }

  @Override
  public int hashCode() {
    return Objects.hashCode(getPropertyName());
  }
}
