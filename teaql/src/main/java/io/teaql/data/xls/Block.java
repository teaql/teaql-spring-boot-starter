package io.teaql.data.xls;

import java.util.HashMap;
import java.util.Map;

public class Block {
  // the page
  private String page;
  // the region
  private int top;
  private int bottom;
  private int left;
  private int right;
  // style references
  private Block styleReferBlock;
  // the value
  private Object value;
  // the properties, styles
  private Map<String, Object> properties;

  public Block(String pPage, int x, int y, Object pValue) {
    page = pPage;
    top = y;
    bottom = y;
    left = x;
    right = x;
    value = pValue;
  }

  public Block(BlockBuildContext pBuildContext, Object pValue) {
    this(pBuildContext.getPage(), pBuildContext.getX(), pBuildContext.getY(), pValue);
  }

  public Block() {}

  public Map<String, Object> getProperties() {
    return properties;
  }

  public void setProperties(Map<String, Object> pProperties) {
    properties = pProperties;
  }

  public String getPage() {
    return page;
  }

  public void setPage(String pPage) {
    page = pPage;
  }

  public int getTop() {
    return top;
  }

  public void setTop(int pTop) {
    top = pTop;
  }

  public int getBottom() {
    return bottom;
  }

  public void setBottom(int pBottom) {
    bottom = pBottom;
  }

  public int getLeft() {
    return left;
  }

  public void setLeft(int pLeft) {
    left = pLeft;
  }

  public int getRight() {
    return right;
  }

  public void setRight(int pRight) {
    right = pRight;
  }

  public Object getValue() {
    return value;
  }

  public void setValue(Object pValue) {
    value = pValue;
  }

  public Block addProperty(String name, Object value) {
    if (properties == null) {
      properties = new HashMap<>();
    }

    properties.put(name, value);
    return this;
  }

  public Block getStyleReferBlock() {
    return styleReferBlock;
  }

  public void setStyleReferBlock(Block pStyleReferBlock) {
    styleReferBlock = pStyleReferBlock;
  }

  public Block style(Block style) {
    styleReferBlock = style;
    return this;
  }
}
