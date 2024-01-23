package io.teaql.data.graphql;

import io.teaql.data.UserContext;

public record GraphQLFetcherParam(
    UserContext userContext, String parentType, String field, Object[] params) {
  public GraphQLFetcherParam(UserContext userContext, String parentType, String field) {
    this(userContext, parentType, field, null);
  }
}
