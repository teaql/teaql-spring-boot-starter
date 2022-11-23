package com.doublechaintech.data;

import cn.hutool.core.bean.BeanUtil;

import java.util.List;

// 实体接口
public interface Entity {
  Long getId();

  void setId(Long id);

  Long getVersion();

  void setVersion(Long id);

  default String typeName() {
    return this.getClass().getSimpleName();
  }

  default Entity save(UserContext userContext) {
    userContext.saveGraph(this);
    return this;
  }

  default void delete(UserContext userContext) {
    userContext.delete(this);
  }

  boolean newItem();

  boolean updateItem();

  boolean deleteItem();

  boolean needPersist();

  default Object getProperty(String propertyName) {
    return BeanUtil.getProperty(this, propertyName);
  }

  default void setProperty(String propertyName, Object value) {
    BeanUtil.setProperty(this, propertyName, value);
  }

  List<String> getUpdatedProperties();

  void addRelation(String relationName, Entity value);

  void addDynamicProperty(String propertyName, Object value);

  void appendDynamicProperty(String propertyName, Object value);

  Object getDynamicProperty(String propertyName);
}
