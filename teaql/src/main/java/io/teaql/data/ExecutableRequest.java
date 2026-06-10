package io.teaql.data;

import java.util.stream.Stream;

/**
 * 已声明 comment 和 purpose 的查询，可以执行。
 * 只能通过 BaseRequest.build() 创建，强制要求 comment + purpose。
 *
 * 设计目的：编译期阻止未声明意图的查询执行。
 *
 * 用法:
 *   // ✅ 编译通过
 *   Q.tasks()
 *       .filterByName("xxx")
 *       .comment("查询任务")
 *       .purpose("展示看板")
 *       .build()              // 返回 ExecutableRequest
 *       .executeForList(ctx); // 只有 ExecutableRequest 才能执行
 *
 *   // ❌ 编译失败：没有 build()，拿不到 ExecutableRequest
 *   Q.tasks().executeForList(ctx);
 */
public class ExecutableRequest<T extends Entity> {
    private final SearchRequest<T> request;

    ExecutableRequest(SearchRequest<T> request) {
        this.request = request;
    }

    public SmartList<T> executeForList(UserContext ctx) {
        return request.executeForList(ctx);
    }

    public T executeForOne(UserContext ctx) {
        return request.executeForOne(ctx);
    }

    public Stream<T> executeForStream(UserContext ctx) {
        return request.executeForStream(ctx);
    }

    public AggregationResult aggregation(UserContext ctx) {
        return request.aggregation(ctx);
    }

    public SearchRequest<T> request() {
        return request;
    }
}
