package io.teaql.graphql;

import io.teaql.data.TQLException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class SimpleGraphqlQueryFactory implements GraphqlQuerySupport {

  Map<String, GraphqlFieldQuery> queries = new ConcurrentHashMap<>();

  @Override
  public GraphqlFieldQuery findFieldQuery(GraphqlFetcherParam param) {
    GraphqlFieldQuery graphqlFieldQuery = queries.get(param.parentType() + ":" + param.field());
    if (graphqlFieldQuery == null) {
      throw new TQLException(
          "Could not find GraphqlFieldQuery for " + param.parentType() + ":" + param.field());
    }
    return graphqlFieldQuery;
  }

  public void register(GraphqlFieldQuery query) {
    queries.put(query.id(), query);
  }
}
