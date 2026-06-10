package io.teaql.data.log;

/**
 * App-layer log sink. Receives masked SQL logs.
 * Application can register custom implementations: display on UI, send elsewhere.
 */
public interface LogSink {

    void onSqlLog(SqlLogEntry entry);
}
