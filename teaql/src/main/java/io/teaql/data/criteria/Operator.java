package io.teaql.data.criteria;

import io.teaql.data.PropertyFunction;

public enum Operator implements PropertyFunction {
  EQUAL,
  NOT_EQUAL,
  GREATER_THAN,
  GREATER_THAN_OR_EQUAL,
  LESS_THAN,
  LESS_THAN_OR_EQUAL,
  END_WITH,
  NOT_END_WITH,
  BEGIN_WITH,
  NOT_BEGIN_WITH,
  CONTAIN,
  NOT_CONTAIN,
  IS_NOT_NULL,
  IS_NULL,
  IN,
  NOT_IN,
  BETWEEN;

  public boolean hasOneOperator(){
    return this == IS_NULL || this == IS_NOT_NULL;
  }

  public boolean hasTwoOperator(){
    return this != IS_NULL && this != IS_NOT_NULL && this != BETWEEN;
  }

  public boolean hasMultiValue(){
    return this == IN || this == NOT_IN;
  }

  public boolean isBetween(){
    return this == BETWEEN;
  }
}
