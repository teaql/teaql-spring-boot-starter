package io.teaql.data.sql.expression;

import java.util.Map;

import io.teaql.data.UserContext;
import io.teaql.data.criteria.RawSql;
import io.teaql.data.sql.SQLRepository;

public class RawSqlParser implements SQLExpressionParser<RawSql> {

    @Override
    public Class<RawSql> type() {
        return RawSql.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            RawSql expression,
            Map<String, Object> parameters,
            SQLRepository sqlColumnResolver) {
        return expression.getSql();
    }
}
