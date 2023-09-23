package io.teaql.data;

public interface RequestHolder {
  String getHeader(String name);

  byte[] getPart(String name);

  String getParameter(String name);

  byte[] getBodyBytes();
}
