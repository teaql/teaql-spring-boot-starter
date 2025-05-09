package io.teaql.data.mysql;

import cn.hutool.core.util.StrUtil;

import io.teaql.data.AggrFunction;
import io.teaql.data.sql.expression.AggrExpressionParser;

public class MysqlAggrExpressionParser extends AggrExpressionParser {
    @Override
    public String genAggrSQL(AggrFunction operator, String sqlColumn) {
        if (operator == AggrFunction.GBK) {
            return StrUtil.format("convert({} using gbk)", sqlColumn);
        }
        return super.genAggrSQL(operator, sqlColumn);
    }
}
