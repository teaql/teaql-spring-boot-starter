package io.teaql.data.graphql;

import graphql.schema.*;

public class TeaQLDataFetcherFactory implements DataFetcherFactory {

  @Override
  public DataFetcher get(DataFetcherFactoryEnvironment environment) {
    GraphQLFieldDefinition fieldDefinition = environment.getFieldDefinition();
    GraphQLOutputType type = fieldDefinition.getType();
    if (GraphQLTypeUtil.isList(type) || GraphQLTypeUtil.isObjectType(type)) {
      return new TeaQLDataFetcher(environment);
    }
    return PropertyDataFetcher.fetching(environment.getFieldDefinition().getName());
  }
}
