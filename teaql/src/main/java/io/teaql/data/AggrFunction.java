package io.teaql.data;

public enum AggrFunction implements PropertyFunction {
  SELF,
  MIN,
  MAX,
  COUNT,
  SUM,
  GBK,
}