package io.teaql.data.graphql;

import io.teaql.data.BaseRequest;
import io.teaql.data.UserContext;

public interface GraphQLFieldQuery {

    String id();

    BaseRequest buildQuery(UserContext userContext, Object[] parameters);

    String getRequestProperty();
}
