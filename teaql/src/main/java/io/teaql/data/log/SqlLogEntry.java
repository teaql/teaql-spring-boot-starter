package io.teaql.data.log;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * SQL execution log. Records details of each SQL operation.
 * Design aligned with teaql-rs SqlLogEntry.
 */
public class SqlLogEntry {

    public enum Operation { SELECT, INSERT, UPDATE, DELETE, RECOVER }

    private final Operation operation;
    private final String sql;
    private final Object[] params;
    private final String debugSql;
    private final String prettySql;
    private final Instant startedAt;
    private final Instant endedAt;
    private final Duration elapsed;
    private final Integer resultCount;
    private final String resultType;
    private final Integer affectedRows;
    private final String resultSummary;
    private final String comment;

    public SqlLogEntry(Operation operation, String sql, Object[] params,
                       String debugSql, String prettySql,
                       Instant startedAt, Instant endedAt, Duration elapsed,
                       Integer resultCount, String resultType, Integer affectedRows,
                       String resultSummary, String comment) {
        this.operation = operation;
        this.sql = sql;
        this.params = params;
        this.debugSql = debugSql;
        this.prettySql = prettySql;
        this.startedAt = startedAt;
        this.endedAt = endedAt;
        this.elapsed = elapsed;
        this.resultCount = resultCount;
        this.resultType = resultType;
        this.affectedRows = affectedRows;
        this.resultSummary = resultSummary;
        this.comment = comment;
    }

    // --- Getter ---

    public Operation getOperation() { return operation; }
    public String getSql() { return sql; }
    public Object[] getParams() { return params; }
    public String getDebugSql() { return debugSql; }
    public String getPrettySql() { return prettySql; }
    public Instant getStartedAt() { return startedAt; }
    public Instant getEndedAt() { return endedAt; }
    public Duration getElapsed() { return elapsed; }
    public Integer getResultCount() { return resultCount; }
    public String getResultType() { return resultType; }
    public Integer getAffectedRows() { return affectedRows; }
    public String getResultSummary() { return resultSummary; }
    public String getComment() { return comment; }

    public boolean isSelect() { return operation == Operation.SELECT; }
    public boolean isMutation() { return !isSelect(); }
}
