package io.teaql.data;

public class DataConfigProperties implements DataConfig {

  private boolean ensureTable;

  private Class<? extends UserContext> contextClass = UserContext.class;

  public boolean isEnsureTable() {
    return ensureTable;
  }

  public void setEnsureTable(boolean pEnsureTable) {
    ensureTable = pEnsureTable;
  }

  public Class<? extends UserContext> getContextClass() {
    return contextClass;
  }

  public void setContextClass(Class<? extends UserContext> pContextClass) {
    contextClass = pContextClass;
  }

  @Override
  public boolean ensureTableEnabled() {
    return isEnsureTable();
  }

  @Override
  public Class<? extends UserContext> contextType() {
    return getContextClass();
  }
}
