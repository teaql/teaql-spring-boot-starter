package io.teaql.data.sqlite;

import io.teaql.data.utils.StrUtil;

import io.teaql.data.AggrFunction;
import io.teaql.data.sql.expression.AggrExpressionParser;

public class SQLiteAggrExpressionParser extends AggrExpressionParser {
    @Override
    public String genAggrSQL(AggrFunction operator, String sqlColumn) {
        if (operator == AggrFunction.GBK) {
            return StrUtil.format("convert({} using gbk)", sqlColumn);
        }
        return super.genAggrSQL(operator, sqlColumn);
    }
}
