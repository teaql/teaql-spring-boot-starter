package com.doublechaintech.data;

/**
 * @author Jackytin 常量表达式
 */
public class Constant implements Expression {
  private Object value;

  public Object getValue() {
    return value;
  }

  public void setValue(Object pValue) {
    value = pValue;
  }
}
