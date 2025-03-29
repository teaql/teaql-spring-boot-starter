package io.teaql.data;

public class DataConfigProperties {

    private boolean ensureTable;

    private String graphqlSchemaFile = "classpath:META-INF/graphql/schema.graphqls";

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

    public String getGraphqlSchemaFile() {
        return graphqlSchemaFile;
    }

    public void setGraphqlSchemaFile(String pGraphqlSchemaFile) {
        graphqlSchemaFile = pGraphqlSchemaFile;
    }
}
