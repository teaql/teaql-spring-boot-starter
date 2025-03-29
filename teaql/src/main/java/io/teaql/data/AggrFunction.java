package io.teaql.data;

public enum AggrFunction implements PropertyFunction {
  SELF,
  MIN,
  MAX,
  AVG,
  COUNT,
  SUM,
  GBK,
  STDDEV,
  STDDEV_POP,
  VAR_SAMP,
  VAR_POP,
  BIT_AND,
  BIT_OR,
  BIT_XOR,
}
