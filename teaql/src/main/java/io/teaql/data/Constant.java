package io.teaql.data;

/**
 * @author Jackytin constant expression
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
