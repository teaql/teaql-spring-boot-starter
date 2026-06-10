package io.teaql.data.android;

import java.util.List;
import java.util.Map;

/**
 * TeaQL 数据库抽象层。
 * Android 端用 SQLiteDatabase 实现，JVM 端用 JDBC 实现。
 * 不依赖 spring-jdbc、javax.sql.DataSource。
 */
public interface TeaQLDatabase {

    /**
     * 执行查询，返回结果列表。每行是一个 Map（列名 → 值）。
     */
    List<Map<String, Object>> query(String sql, Object[] args);

    /**
     * 执行更新（INSERT/UPDATE/DELETE），返回影响行数。
     */
    int executeUpdate(String sql, Object[] args);

    /**
     * 批量执行更新。
     */
    int[] batchUpdate(String sql, List<Object[]> batchArgs);

    /**
     * 执行任意 SQL（DDL 等）。
     */
    void execute(String sql);

    /**
     * 在事务中执行操作。
     */
    void executeInTransaction(Runnable action);

    /**
     * 获取数据库表的列信息。
     */
    List<Map<String, Object>> getTableColumns(String tableName);
}
