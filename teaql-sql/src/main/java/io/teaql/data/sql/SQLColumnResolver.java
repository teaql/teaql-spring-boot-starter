package io.teaql.data.sql;

import java.util.List;
import java.util.Map;

import io.teaql.data.SearchRequest;
import io.teaql.data.UserContext;
import io.teaql.data.utils.CollUtil;
import io.teaql.data.sql.expression.SQLExpressionParser;

public interface SQLColumnResolver {

    default SQLColumn getPropertyColumn(String idTable, String property) {
        return CollUtil.getFirst(getPropertyColumns(idTable, property));
    }

    List<SQLColumn> getPropertyColumns(String idTable, String property);

    default Map<Class, SQLExpressionParser> getExpressionParsers() {
        return java.util.Collections.emptyMap();
    }

    default boolean canMixinSubQuery(UserContext userContext, SearchRequest subQuery) {
        return false;
    }
}
