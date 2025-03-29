package io.teaql.data;

public interface ResponseHolder {
  void setHeader(String name, String value);

  String getHeader(String name);
}
