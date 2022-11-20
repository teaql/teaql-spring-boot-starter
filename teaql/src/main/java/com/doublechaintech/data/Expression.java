package com.doublechaintech.data;

import java.util.Map;

/**
 * @author Jackytin 表达式，顶级接口
 */
public interface Expression extends PropertyAware {
  private String nextPropertyKey(Map<String, Object> parameters, String propertyName) {
    while (parameters.containsKey(propertyName)) {
      propertyName = genNextKey(propertyName);
    }
    return propertyName;
  }

  private String genNextKey(String key) {
    char c = key.charAt(key.length() - 1);
    if (!Character.isDigit(c)) {
      return key + "0";
    } else {
      return key.substring(0, key.length() - 1) + (char) (c + 1);
    }
  }
}
