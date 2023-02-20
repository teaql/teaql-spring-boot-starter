package io.teaql.data.criteria;

import io.teaql.data.Expression;

public class RawSql implements Expression {
    String sql;

    public RawSql(String pSql) {
        sql = pSql;
    }

    public String getSql() {
        return sql;
    }
}
