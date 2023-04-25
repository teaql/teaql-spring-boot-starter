package io.teaql.data;

public interface DataConfig {
  boolean ensureTableEnabled();

  Class<? extends UserContext> contextType();
}
