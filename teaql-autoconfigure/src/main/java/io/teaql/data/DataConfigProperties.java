package io.teaql.data;

public class DataConfigProperties implements DataConfig {

  private boolean ensureTable;

  public boolean isEnsureTable() {
    return ensureTable;
  }

  public void setEnsureTable(boolean pEnsureTable) {
    ensureTable = pEnsureTable;
  }

  @Override
  public boolean ensureTableEnabled() {
    return isEnsureTable();
  }
}
