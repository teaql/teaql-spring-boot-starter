package io.teaql.graphql;

public class RootQueryType extends BaseQueryContainer {
  @Override
  protected String type() {
    return "Query";
  }
}
