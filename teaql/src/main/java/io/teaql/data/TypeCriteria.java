package io.teaql.data;

public class TypeCriteria implements SearchCriteria {
  private Parameter typeParameter;

  public TypeCriteria(Parameter pTypeParameter) {
    typeParameter = pTypeParameter;
  }

  public TypeCriteria() {}

  public Parameter getTypeParameter() {
    return typeParameter;
  }

  public void setTypeParameter(Parameter pTypeParameter) {
    typeParameter = pTypeParameter;
  }
}
