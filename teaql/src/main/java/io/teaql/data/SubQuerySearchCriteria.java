package io.teaql.data;

import cn.hutool.core.collection.ListUtil;
import java.util.List;

public class SubQuerySearchCriteria implements SearchCriteria, PropertyAware {
  private String propertyName;
  private SearchRequest dependsOn;
  private String dependsOnPropertyName;

  public SubQuerySearchCriteria(
      String pPropertyName, SearchRequest pDependsOn, String pDependsOnPropertyName) {
    propertyName = pPropertyName;
    dependsOn = pDependsOn;
    dependsOnPropertyName = pDependsOnPropertyName;
  }

  public String getPropertyName() {
    return propertyName;
  }

  public void setPropertyName(String pPropertyName) {
    propertyName = pPropertyName;
  }

  public SearchRequest getDependsOn() {
    return dependsOn;
  }

  public void setDependsOn(SearchRequest pDependsOn) {
    dependsOn = pDependsOn;
  }

  public String getDependsOnPropertyName() {
    return dependsOnPropertyName;
  }

  public void setDependsOnPropertyName(String pDependsOnPropertyName) {
    dependsOnPropertyName = pDependsOnPropertyName;
  }

  @Override
  public List<String> properties(UserContext ctx) {
    return ListUtil.of(propertyName);
  }
}
