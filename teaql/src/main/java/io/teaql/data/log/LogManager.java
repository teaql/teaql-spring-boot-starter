package io.teaql.data.log;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

import io.teaql.data.UserContext;

/**
 * 双层日志管理器。
 *
 * ═══════════════════════════════════════════════════════
 * Layer 1: Runtime 层 (强制，环境变量控制，不可定制)
 * ═══════════════════════════════════════════════════════
 *   包含原始未脱敏的 SQL 日志和审计信息。
 *   通过环境变量控制，无代码：
 *     TEAQL_LOG_ENDPOINT  - 文件路径 (空则不写)
 *     TEAQL_LOG_FORMAT    - human (默认) | json
 *     TEAQL_LOG_SELECT    - true 记录 SELECT (默认 false)
 *     TEAQL_LOG_MUTATION  - false 不记录变更 (默认 true)
 *   写入的审计信息包含原始字段值，不脱敏。
 *
 * ═══════════════════════════════════════════════════════
 * Layer 2: App 层 (可定制)
 * ═══════════════════════════════════════════════════════
 *   应用层收到的是脱敏后的日志和审计事件。
 *   可以注册 LogSink 和 AuditSink 来定制行为：
 *     - 显示到界面
 *     - 发送到消息队列
 *     - 写入数据库
 */
public class LogManager {

    // ==========================================
    // Layer 1: Runtime 层 (环境变量，只读，不可定制)
    // ==========================================

    private static final String LOG_ENDPOINT = System.getenv("TEAQL_LOG_ENDPOINT");
    private static final LogFormatter FORMATTER = LogFormatter.getFormatter();
    private static final boolean MUTATION_LOG_ENABLED = !"false".equals(System.getenv("TEAQL_LOG_MUTATION"));
    private static final boolean SELECT_LOG_ENABLED = "true".equals(System.getenv("TEAQL_LOG_SELECT"));

    /**
     * Runtime 层: 记录 SQL 日志 (原始未脱敏)。
     * 由 Repository 层调用，写入 TEAQL_LOG_ENDPOINT 文件。
     */
    public void recordSqlLog(SqlLogEntry entry) {
        if (LOG_ENDPOINT == null || LOG_ENDPOINT.isEmpty()) return;
        if (entry.isSelect() && !SELECT_LOG_ENABLED) return;
        if (entry.isMutation() && !MUTATION_LOG_ENABLED) return;

        String traceChain = entry.getComment() != null ? entry.getComment() : "";
        String content = FORMATTER.formatSqlLog(traceChain, entry);
        writeToEndpoint(content);

        // 同时分发到 App 层 (脱敏后)
        SqlLogEntry safeEntry = sanitizeSqlLog(entry);
        for (LogSink sink : logSinks) {
            sink.onSqlLog(safeEntry);
        }
    }

    /**
     * Runtime 层: 记录审计事件 (原始未脱敏)。
     * 由 Repository 层调用，写入 TEAQL_LOG_ENDPOINT 文件。
     *
     * @param maskFields 需要脱敏的字段 (来自 EntityDescriptor.audit_mask_fields)
     * @param maxValueLen 值最大长度 (来自 EntityDescriptor.audit_value_max_len)
     */
    public void emitAuditEvent(UserContext ctx, AuditEvent event,
                                List<String> maskFields, Integer maxValueLen) {
        // Layer 1: 写文件 (原始未脱敏)
        if (LOG_ENDPOINT != null && !LOG_ENDPOINT.isEmpty()) {
            String content = FORMATTER.formatAuditLog(event);
            writeToEndpoint(content);
        }

        // Layer 2: 分发到 App 层 (脱敏后)
        AuditEvent safeEvent = sanitizeAuditEvent(event, maskFields, maxValueLen);
        for (AuditEventSink sink : auditSinks) {
            sink.onEvent(ctx, safeEvent);
        }
    }

    /**
     * 便捷方法: 无脱敏配置时使用默认脱敏。
     */
    public void emitAuditEvent(UserContext ctx, AuditEvent event) {
        emitAuditEvent(ctx, event, List.of(), null);
    }

    // ==========================================
    // 脱敏处理 (对标 Rust 的 build_safe_event)
    // ==========================================

    /**
     * 脱敏 SQL 日志。
     */
    private SqlLogEntry sanitizeSqlLog(SqlLogEntry entry) {
        Object[] safeParams = entry.getParams();
        if (safeParams != null) {
            safeParams = safeParams.clone();
            for (int i = 0; i < safeParams.length; i++) {
                if (safeParams[i] instanceof String s && s.length() > 100) {
                    safeParams[i] = s.substring(0, 20) + "...(truncated)";
                }
            }
        }
        return new SqlLogEntry(
            entry.getOperation(), entry.getSql(), safeParams,
            entry.getDebugSql(), entry.getPrettySql(),
            entry.getStartedAt(), entry.getEndedAt(), entry.getElapsed(),
            entry.getResultCount(), entry.getResultType(), entry.getAffectedRows(),
            entry.getResultSummary(), entry.getComment());
    }

    /**
     * 脱敏审计事件。使用 EntityDescriptor 上的 audit_mask_fields 和 audit_value_max_len。
     * 对标 Rust 的 RawAuditEvent::build_safe_event。
     */
    private AuditEvent sanitizeAuditEvent(AuditEvent event, List<String> maskFields, Integer maxValueLen) {
        Map<String, Object> safeValues = maskMap(event.getValues(), maskFields, maxValueLen);
        Map<String, Object> safeOldValues = maskMap(event.getOldValues(), maskFields, maxValueLen);
        Map<String, Object> safeNewValues = maskMap(event.getNewValues(), maskFields, maxValueLen);

        List<AuditEvent.PropertyChange> safeChanges = new ArrayList<>();
        for (AuditEvent.PropertyChange change : event.getChanges()) {
            safeChanges.add(new AuditEvent.PropertyChange(
                change.getField(),
                maskValue(change.getField(), change.getOldValue(), maskFields, maxValueLen),
                maskValue(change.getField(), change.getNewValue(), maskFields, maxValueLen)));
        }

        return new AuditEvent(
            event.getKind(), event.getEntity(), safeValues,
            event.getUpdatedFields(), safeOldValues, safeNewValues,
            safeChanges, event.getComment());
    }

    private Map<String, Object> maskMap(Map<String, Object> map, List<String> maskFields, Integer maxLen) {
        if (map == null) return null;
        Map<String, Object> result = new java.util.HashMap<>();
        for (Map.Entry<String, Object> e : map.entrySet()) {
            result.put(e.getKey(), maskValue(e.getKey(), e.getValue(), maskFields, maxLen));
        }
        return result;
    }

    /**
     * 单字段脱敏。对标 Rust 的 mask_audit_value + limit_audit_value。
     *
     * @param field     字段名
     * @param value     原始值
     * @param maskFields 需要脱敏的字段列表 (来自 EntityDescriptor.audit_mask_fields)
     * @param maxLen    值最大长度 (来自 EntityDescriptor.audit_value_max_len)
     */
    private Object maskValue(String field, Object value, List<String> maskFields, Integer maxLen) {
        if (value == null) return null;

        String s = String.valueOf(value);

        // 1. 字段级脱敏 (对标 Rust: should_mask = audit_mask_fields.contains(field))
        if (maskFields.contains(field)) {
            return maskAuditValue(s);
        }

        // 2. 长度截断 (对标 Rust: limit_audit_value)
        if (maxLen != null && s.length() > maxLen) {
            return limitAuditValue(s, maxLen);
        }

        return value;
    }

    /**
     * 脱敏值。对标 Rust 的 mask_audit_value。
     * 数字: 全部替换为 *
     * 短字符串: 全部替换为 *
     * 长字符串: 保留前2后2，中间替换为 *
     */
    private String maskAuditValue(String value) {
        if (value.isEmpty()) return "";

        if (value.chars().allMatch(Character::isDigit)) {
            return "*".repeat(value.length());
        }

        if (value.length() < 8) {
            return "*".repeat(value.length());
        }

        String prefix = value.substring(0, 2);
        String suffix = value.substring(value.length() - 2);
        String middle = "*".repeat(value.length() - 4);
        return prefix + middle + suffix;
    }

    /**
     * 截断值。对标 Rust 的 limit_audit_value。
     * 保留前半和后半，中间用 ... 连接。
     */
    private String limitAuditValue(String value, int maxLen) {
        if (value.length() <= maxLen) return value;
        if (maxLen <= 3) return "*".repeat(maxLen);

        int keepLen = maxLen - 3; // "..." length
        int headLen = keepLen / 2;
        int tailLen = keepLen - headLen;

        String head = value.substring(0, headLen);
        String tail = value.substring(value.length() - tailLen);
        return head + "..." + tail;
    }

    // ==========================================
    // Layer 2: App 层 (可定制)
    // ==========================================

    private final List<LogSink> logSinks = new CopyOnWriteArrayList<>();
    private final List<AuditEventSink> auditSinks = new CopyOnWriteArrayList<>();

    /**
     * App 层: 注册日志 Sink (接收脱敏后的日志)。
     */
    public void addLogSink(LogSink sink) {
        logSinks.add(sink);
    }

    /**
     * App 层: 注册审计 Sink (接收脱敏后的审计事件)。
     */
    public void addAuditSink(AuditEventSink sink) {
        auditSinks.add(sink);
    }

    public List<AuditEventSink> getAuditSinks() {
        return new ArrayList<>(auditSinks);
    }

    public List<LogSink> getLogSinks() {
        return new ArrayList<>(logSinks);
    }

    // ==========================================
    // 文件输出 (Layer 1)
    // ==========================================

    private static void writeToEndpoint(String content) {
        if (LOG_ENDPOINT == null) return;
        try (PrintWriter writer = new PrintWriter(new FileWriter(LOG_ENDPOINT, true))) {
            writer.println(content);
        } catch (IOException ignored) {
        }
    }
}
