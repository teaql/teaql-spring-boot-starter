package io.teaql.data.mysql;

import io.teaql.data.criteria.Operator;
import io.teaql.data.sql.expression.ParameterParser;

public class MysqlParameterParser extends ParameterParser {
  @Override
  public Object fixValue(Operator operator, Object pValue) {
    switch (operator) {
      case IN_LARGE:
      case NOT_IN_LARGE:
        return pValue;
    }
    return super.fixValue(operator, pValue);
  }
}
