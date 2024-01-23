package io.teaql.data.graphql;

import io.teaql.data.DataConfigProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class GraphQLConfiguration {

  @Bean
  public GraphQLService graphqlService(DataConfigProperties config) {
    return new GraphQLService(config);
  }

  @Bean
  public GraphQLSupport graphqlQuerySupport(BaseQueryContainer[] containers) {
    SimpleGraphQLFactory simpleGraphqlQueryFactory = new SimpleGraphQLFactory();
    if (containers != null) {
      for (BaseQueryContainer container : containers) {
        container.register(simpleGraphqlQueryFactory);
      }
    }
    return simpleGraphqlQueryFactory;
  }
}
