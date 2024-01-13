package io.teaql.graphql;

import graphql.schema.*;

public class TeaqlDataFetcherFactory implements DataFetcherFactory {

  @Override
  public DataFetcher get(DataFetcherFactoryEnvironment environment) {
    GraphQLFieldDefinition fieldDefinition = environment.getFieldDefinition();
    GraphQLOutputType type = fieldDefinition.getType();
    if (GraphQLTypeUtil.isList(type) || GraphQLTypeUtil.isObjectType(type)) {
      return new TeaqlDataFetcher(environment);
    }
    return env -> PropertyDataFetcher.fetching(env.getFieldDefinition().getName());
  }
}
