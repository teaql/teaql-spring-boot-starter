package io.teaql.graphql;

import io.teaql.data.UserContext;

public record GraphqlFetcherParam(
    UserContext userContext, String parentType, String field, Object[] params) {
  public GraphqlFetcherParam(UserContext userContext, String parentType, String field) {
    this(userContext, parentType, field, null);
  }
}
