package io.teaql.data.log;

import io.teaql.data.UserContext;

/**
 * 审计事件接收器接口。可插拔实现。
 * 设计参考 teaql-rs 的 RawAuditEventSink。
 *
 * 实现示例:
 *   - 数据库存储
 *   - 消息队列发送
 *   - 文件写入
 *   - 控制台输出
 */
public interface AuditEventSink {

    void onEvent(UserContext ctx, AuditEvent event);
}
