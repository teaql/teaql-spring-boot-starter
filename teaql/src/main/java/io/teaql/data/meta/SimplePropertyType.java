package io.teaql.data.meta;

/** 基本的属性类型 */
public class SimplePropertyType implements PropertyType {

  private Class javaType;

  public SimplePropertyType(Class pJavaType) {
    javaType = pJavaType;
  }

  @Override
  public Class javaType() {
    return javaType;
  }
}
