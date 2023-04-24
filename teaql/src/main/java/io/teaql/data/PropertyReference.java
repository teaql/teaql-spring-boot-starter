package io.teaql.data;

import cn.hutool.core.collection.ListUtil;

import java.util.List;

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
}
