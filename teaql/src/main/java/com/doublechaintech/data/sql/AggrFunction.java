package com.doublechaintech.data.sql;

import cn.hutool.core.util.StrUtil;
import com.doublechaintech.data.PropertyFunction;

public interface AggrFunction extends PropertyFunction {
  String toString(String field);

  AggrFunction SELF = field -> field;
  AggrFunction MIN = field -> StrUtil.format("min({})", field);
  AggrFunction MAX = field -> StrUtil.format("max({})", field);
  AggrFunction COUNT = field -> StrUtil.format("count({})", field);
  AggrFunction SUM = field -> StrUtil.format("sum({})", field);
  AggrFunction GBK = field -> StrUtil.format("convert({} using gbk)", field);
}
