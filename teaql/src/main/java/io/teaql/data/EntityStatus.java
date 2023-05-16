package io.teaql.data;

import static io.teaql.data.EntityAction.*;

import cn.hutool.core.map.multi.RowKeyTable;
import cn.hutool.core.util.StrUtil;

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

  // 已更新, 对于PERSISTED_DELETED的对象，调用recover
  UPDATED_RECOVER,

  // 引用, 含有ID, 只表示关系， 持久化时会跳过它
  REFER;

  private static final RowKeyTable<EntityStatus, EntityAction, EntityStatus> statusTransaction =
      new RowKeyTable<>();

  static {
    statusTransaction.put(NEW, UPDATE, NEW);
    statusTransaction.put(NEW, PERSIST, PERSISTED);
    statusTransaction.put(PERSISTED, UPDATE, UPDATED);
    statusTransaction.put(PERSISTED, DELETE, UPDATED_DELETED);
    statusTransaction.put(PERSISTED_DELETED, RECOVER, UPDATED_RECOVER);
    statusTransaction.put(UPDATED, UPDATE, UPDATED);
    statusTransaction.put(UPDATED, PERSIST, PERSISTED);
    statusTransaction.put(UPDATED_DELETED, PERSIST, PERSISTED_DELETED);
    statusTransaction.put(UPDATED_DELETED, DELETE, UPDATED_DELETED);
    statusTransaction.put(UPDATED_RECOVER, PERSIST, PERSISTED);
    statusTransaction.put(UPDATED_RECOVER, RECOVER, UPDATED_RECOVER);
  }

  public EntityStatus next(EntityAction action) {
    EntityStatus entityStatus = statusTransaction.get(this, action);
    if (entityStatus == null) {
      throw new RepositoryException(
          StrUtil.format("current status: {} cannot apply action: {}", this, action));
    }
    return entityStatus;
  }
}
