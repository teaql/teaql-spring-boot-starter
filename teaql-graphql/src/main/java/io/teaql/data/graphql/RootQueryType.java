package io.teaql.data.graphql;

public class RootQueryType extends BaseQueryContainer {
  @Override
  protected String type() {
    return "Query";
  }
}
