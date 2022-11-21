package com.doublechaintech.data.sql.expression;

import com.doublechaintech.data.UserContext;
import com.doublechaintech.data.criteria.RawSql;
import com.doublechaintech.data.sql.SQLColumnResolver;

import java.util.Map;

public class RawSqlParser implements SQLExpressionParser<RawSql> {

    @Override
    public Class<RawSql> type() {
        return RawSql.class;
    }

    @Override
    public String toSql(UserContext userContext, RawSql expression, Map<String, Object> parameters, SQLColumnResolver sqlColumnResolver) {
        return expression.getSql();
    }
}
