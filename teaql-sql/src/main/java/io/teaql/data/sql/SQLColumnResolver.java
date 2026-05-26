package io.teaql.data.sql;

import java.util.List;

import io.teaql.data.utils.CollUtil;

public interface SQLColumnResolver {

    default SQLColumn getPropertyColumn(String idTable, String property) {
        return CollUtil.getFirst(getPropertyColumns(idTable, property));
    }

    List<SQLColumn> getPropertyColumns(String idTable, String property);
}
