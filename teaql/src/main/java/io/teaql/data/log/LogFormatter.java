package io.teaql.data.log;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.stream.Collectors;

/**
 * 日志格式化器。支持两种格式：
 * - HumanReaderFormatter: 人类可读格式
 * - DebugReaderFormatter: 机器可读格式
 *
 * 设计参考 teaql-rs 的 LogFormatter trait。
 */
public interface LogFormatter {

    String formatSqlLog(String traceChain, SqlLogEntry entry);
    String formatAuditLog(AuditEvent event);

    // --- 人类可读格式 ---
    LogFormatter HUMAN = new HumanReaderFormatter();

    // --- 机器可读格式 ---
    LogFormatter DEBUG = new DebugReaderFormatter();

    /**
     * 根据环境变量选择格式化器。
     */
    static LogFormatter getFormatter() {
        String format = System.getenv("TEAQL_LOG_FORMAT");
        if ("json".equals(format) || "debug".equals(format)) {
            return DEBUG;
        }
        return HUMAN;
    }

    /**
     * 人类可读格式化器。
     */
    class HumanReaderFormatter implements LogFormatter {
        private static final DateTimeFormatter TS =
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS").withZone(ZoneId.systemDefault());

        @Override
        public String formatSqlLog(String traceChain, SqlLogEntry entry) {
            String ts = TS.format(entry.getStartedAt());
            long elapsedUs = entry.getElapsed().toNanos() / 1000;
            String trace = traceChain.isEmpty() ? "" : " - [" + traceChain + "]";
            return String.format("[%s]-[%5dµs]-[DEBUG]-SqlLogEntry%s - [%s]\n          %s",
                ts, elapsedUs, trace, entry.getResultSummary(), entry.getPrettySql().replace("\n", " "));
        }

        @Override
        public String formatAuditLog(AuditEvent event) {
            String ts = TS.format(Instant.now());
            String trace = event.getComment() != null ? " (Trace: " + event.getComment() + ")" : "";

            String fields = event.getChanges().stream()
                .filter(c -> !c.getField().startsWith("_"))
                .map(c -> c.getField() + ": " + (c.getNewValue() != null ? c.getNewValue() : "null"))
                .collect(Collectors.joining(", "));
            String fieldsPart = fields.isEmpty() ? "" : " {" + fields + "}";

            Object entityId = event.getValues() != null ? event.getValues().get("id") : "Unknown";

            return String.format("[%s]-[AUDIT]-Entity [%s:%s] %s%s%s",
                ts, event.getEntity(), entityId, event.getKind(), trace, fieldsPart);
        }
    }

    /**
     * 机器可读格式化器。
     */
    class DebugReaderFormatter implements LogFormatter {
        @Override
        public String formatSqlLog(String traceChain, SqlLogEntry entry) {
            return String.format("[SQL_LOG] %s - Entry: {op=%s, sql=%s, result=%s}",
                traceChain, entry.getOperation(), entry.getDebugSql(), entry.getResultSummary());
        }

        @Override
        public String formatAuditLog(AuditEvent event) {
            return String.format("[AUDIT_LOG] %s - Event: {kind=%s, entity=%s, changes=%d}",
                event.getComment(), event.getKind(), event.getEntity(), event.getChanges().size());
        }
    }
}
