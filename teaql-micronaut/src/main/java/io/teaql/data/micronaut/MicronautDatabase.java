package io.teaql.data.micronaut;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import io.teaql.data.TeaQLDatabase;

/**
 * Micronaut 数据库实现。使用标准 JDBC DataSource。
 * 不依赖 spring-jdbc，可用于 Micronaut / 任何标准 JDBC 环境。
 */
public class MicronautDatabase implements TeaQLDatabase {

    private final DataSource dataSource;

    public MicronautDatabase(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public List<Map<String, Object>> query(String sql, Object[] args) {
        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {
            setParameters(ps, args);
            try (ResultSet rs = ps.executeQuery()) {
                return resultSetToList(rs);
            }
        } catch (SQLException e) {
            throw new RuntimeException("Query failed: " + sql, e);
        }
    }

    @Override
    public int executeUpdate(String sql, Object[] args) {
        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {
            setParameters(ps, args);
            return ps.executeUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Update failed: " + sql, e);
        }
    }

    @Override
    public int[] batchUpdate(String sql, List<Object[]> batchArgs) {
        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {
            for (Object[] args : batchArgs) {
                setParameters(ps, args);
                ps.addBatch();
            }
            return ps.executeBatch();
        } catch (SQLException e) {
            throw new RuntimeException("Batch update failed: " + sql, e);
        }
    }

    @Override
    public void execute(String sql) {
        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {
            ps.execute();
        } catch (SQLException e) {
            throw new RuntimeException("Execute failed: " + sql, e);
        }
    }

    @Override
    public void executeInTransaction(Runnable action) {
        try (Connection conn = dataSource.getConnection()) {
            boolean autoCommit = conn.getAutoCommit();
            conn.setAutoCommit(false);
            try {
                action.run();
                conn.commit();
            } catch (Exception e) {
                conn.rollback();
                throw e;
            } finally {
                conn.setAutoCommit(autoCommit);
            }
        } catch (SQLException e) {
            throw new RuntimeException("Transaction failed", e);
        }
    }

    @Override
    public List<Map<String, Object>> getTableColumns(String tableName) {
        String sql = String.format(
            "SELECT * FROM information_schema.columns WHERE table_name = '%s'", tableName);
        try {
            return query(sql, new Object[0]);
        } catch (Exception e) {
            return List.of();
        }
    }

    private void setParameters(PreparedStatement ps, Object[] args) throws SQLException {
        if (args == null) return;
        for (int i = 0; i < args.length; i++) {
            ps.setObject(i + 1, args[i]);
        }
    }

    private List<Map<String, Object>> resultSetToList(ResultSet rs) throws SQLException {
        List<Map<String, Object>> rows = new ArrayList<>();
        ResultSetMetaData meta = rs.getMetaData();
        int columnCount = meta.getColumnCount();

        while (rs.next()) {
            Map<String, Object> row = new HashMap<>();
            for (int i = 1; i <= columnCount; i++) {
                row.put(meta.getColumnLabel(i), rs.getObject(i));
            }
            rows.add(row);
        }
        return rows;
    }
}
