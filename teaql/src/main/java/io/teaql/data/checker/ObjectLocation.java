package io.teaql.data.checker;

public class ObjectLocation {
  private ObjectLocation parent;

  public ObjectLocation(ObjectLocation pParent) {
    parent = pParent;
  }

  public ObjectLocation getParent() {
    return parent;
  }

  public static ObjectLocation hashRoot(String memberName) {
    return new HashLocation(null, memberName);
  }

  public static ObjectLocation arrayRoot(int index) {
    return new ArrayLocation(null, index);
  }

  public ObjectLocation member(String memberName) {
    return new HashLocation(this, memberName);
  }

  public ObjectLocation element(int index) {
    return new ArrayLocation(this, index);
  }
}
