package io.teaql.data;

import io.teaql.data.web.UserContextInitializer;

public class DataConfigProperties {

  private boolean ensureTable;

  private Class<? extends UserContext> contextClass = UserContext.class;

  private Class<? extends UserContextInitializer> contextInitializer;

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

  public Class<? extends UserContextInitializer> getContextInitializer() {
    return contextInitializer;
  }

  public void setContextInitializer(Class<? extends UserContextInitializer> pContextInitializer) {
    contextInitializer = pContextInitializer;
  }
}
