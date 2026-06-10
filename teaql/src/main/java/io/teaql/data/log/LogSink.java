package io.teaql.data.log;

/**
 * App 层日志 Sink。接收脱敏后的 SQL 日志。
 * 应用层可注册自定义实现：显示到界面、发送到别处。
 */
public interface LogSink {

    void onSqlLog(SqlLogEntry entry);
}
