package io.teaql.data.meta;

/** basic java property type */
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
