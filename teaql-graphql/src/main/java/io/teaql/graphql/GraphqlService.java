package io.teaql.graphql;

import static graphql.schema.idl.RuntimeWiring.newRuntimeWiring;

import cn.hutool.core.io.resource.ResourceUtil;
import cn.hutool.core.map.MapUtil;
import graphql.ExecutionInput;
import graphql.ExecutionResult;
import graphql.GraphQL;
import graphql.scalars.ExtendedScalars;
import graphql.schema.GraphQLCodeRegistry;
import graphql.schema.GraphQLSchema;
import graphql.schema.idl.*;
import io.teaql.data.DataConfigProperties;
import io.teaql.data.TQLException;
import io.teaql.data.UserContext;

public class GraphqlService implements io.teaql.data.GraphqlService {

  GraphQL graphQL;

  public GraphqlService(DataConfigProperties config) {
    SchemaParser schemaParser = new SchemaParser();
    String graphqlSchemaFile = config.getGraphqlSchemaFile();
    RuntimeWiring runtimeWiring =
        newRuntimeWiring()
            .codeRegistry(
                GraphQLCodeRegistry.newCodeRegistry()
                    .defaultDataFetcher(new TeaqlDataFetcherFactory()))
            .scalar(ExtendedScalars.GraphQLBigDecimal)
            .scalar(ExtendedScalars.GraphQLLong)
            .scalar(ExtendedScalars.Date)
            .scalar(ExtendedScalars.Time)
            .scalar(ExtendedScalars.LocalTime)
            .build();
    SchemaGenerator schemaGenerator = new SchemaGenerator();
    TypeDefinitionRegistry typeDefinitionRegistry =
        schemaParser.parse(ResourceUtil.readUtf8Str(graphqlSchemaFile));
    schemaGenerator.makeExecutableSchema(typeDefinitionRegistry, runtimeWiring);
    GraphQLSchema graphQLSchema =
        schemaGenerator.makeExecutableSchema(typeDefinitionRegistry, runtimeWiring);
    graphQL = GraphQL.newGraphQL(graphQLSchema).build();
  }

  @Override
  public Object execute(UserContext ctx, String query) {
    ExecutionResult result =
        graphQL.execute(
            ExecutionInput.newExecutionInput()
                .graphQLContext(MapUtil.of("userContext", new UserContext()))
                .query(query));
    if (result.getErrors() != null && !result.getErrors().isEmpty()) {
      throw new TQLException(result.getErrors().toString());
    }
    return result.getData();
  }
}
