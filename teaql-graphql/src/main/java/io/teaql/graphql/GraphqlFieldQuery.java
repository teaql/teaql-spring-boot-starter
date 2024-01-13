package io.teaql.graphql;

import io.teaql.data.BaseRequest;
import io.teaql.data.UserContext;

public interface GraphqlFieldQuery {

  String id();

  BaseRequest buildQuery(UserContext userContext, Object[] parameters);

  String getRequestProperty();
}
