package com.doublechaintech.data.criteria;

import com.doublechaintech.data.Expression;

public class RawSql implements Expression {
    String sql;

    public RawSql(String pSql) {
        sql = pSql;
    }

    public String getSql() {
        return sql;
    }
}
