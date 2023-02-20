package io.teaql.data;

// entity的状态
public enum EntityStatus {
  // 通过new 创建的entity, 目标是保存新对象
  NEW,

  // 持久化的
  // Repository接口(save,query)返回的对象（总是含有id, version > 0）
  PERSISTED,

  // Repository接口(save,query,deleted)返回的对象总是含有id, version < 0）
  PERSISTED_DELETED,

  // 已更新, 对于PERSISTED的对象，（成功）更新里面的字段后的状态
  UPDATED,

  // 已删除, 对于PERSISTED的对象
  UPDATED_DELETED,

  // 引用, 含有ID, 只表示关系， 持久化时会跳过它
  REFER
}
