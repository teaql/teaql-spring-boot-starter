package io.teaql.data.sql;

import cn.hutool.core.collection.CollUtil;

import java.util.List;

public interface SQLColumnResolver {

  default SQLColumn getPropertyColumn(String idTable, String property) {
    return CollUtil.getFirst(getPropertyColumns(idTable, property));
  }

  List<SQLColumn> getPropertyColumns(String idTable, String property);
}
