package io.teaql.data;

/**
 * 请求策略接口，在每个操作前调用。
 * 可以修改查询/命令，或拒绝执行。
 *
 * 设计参考 teaql-rs 的 RequestPolicy trait。
 */
public interface RequestPolicy {

    /**
     * 查询前的策略检查。
     * 如果 comment 或 purpose 为空，可以拒绝执行。
     */
    default void enforceSelect(UserContext ctx, SearchRequest<?> query) {
    }

    /**
     * 插入前的策略检查。
     */
    default void enforceInsert(UserContext ctx, Entity entity) {
    }

    /**
     * 更新前的策略检查。
     */
    default void enforceUpdate(UserContext ctx, Entity entity) {
    }

    /**
     * 删除前的策略检查。
     */
    default void enforceDelete(UserContext ctx, Entity entity) {
    }

    /**
     * 恢复前的策略检查。
     */
    default void enforceRecover(UserContext ctx, Entity entity) {
    }
}
