package io.teaql.graphql;

import io.teaql.data.DataConfigProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class GraphqlConfiguration {

  @Bean
  public GraphqlService graphqlService(DataConfigProperties config) {
    return new GraphqlService(config);
  }

  @Bean
  public GraphqlQuerySupport graphqlQuerySupport(BaseQueryContainer[] containers) {
    SimpleGraphqlQueryFactory simpleGraphqlQueryFactory = new SimpleGraphqlQueryFactory();
    if (containers != null) {
      for (BaseQueryContainer container : containers) {
        container.register(simpleGraphqlQueryFactory);
      }
    }
    return simpleGraphqlQueryFactory;
  }
}
