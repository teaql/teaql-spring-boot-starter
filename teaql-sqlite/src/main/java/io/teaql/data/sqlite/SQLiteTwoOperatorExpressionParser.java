package io.teaql.data.sqlite;

import io.teaql.data.criteria.Operator;
import io.teaql.data.sql.expression.TwoOperatorExpressionParser;

public class SQLiteTwoOperatorExpressionParser extends TwoOperatorExpressionParser {
    @Override
    public String getOp(Operator operator) {
        switch (operator) {
            case IN_LARGE:
                return "IN";
            case NOT_IN_LARGE:
                return "NOT IN";
        }
        return super.getOp(operator);
    }
}
