package io.teaql.data.graphql;

import cn.hutool.core.io.resource.ResourceUtil;
import cn.hutool.core.map.MapUtil;

import static graphql.schema.idl.RuntimeWiring.newRuntimeWiring;

import graphql.ExecutionInput;
import graphql.ExecutionResult;
import graphql.GraphQL;
import graphql.scalars.ExtendedScalars;
import graphql.schema.GraphQLCodeRegistry;
import graphql.schema.GraphQLSchema;
import graphql.schema.idl.RuntimeWiring;
import graphql.schema.idl.SchemaGenerator;
import graphql.schema.idl.SchemaParser;
import graphql.schema.idl.TypeDefinitionRegistry;
import io.teaql.data.DataConfigProperties;
import io.teaql.data.TQLException;
import io.teaql.data.UserContext;

public class GraphQLService implements io.teaql.data.GraphQLService {

    GraphQL graphQL;

    public GraphQLService(DataConfigProperties config) {
        SchemaParser schemaParser = new SchemaParser();
        String graphqlSchemaFile = config.getGraphqlSchemaFile();
        RuntimeWiring runtimeWiring =
                newRuntimeWiring()
                        .codeRegistry(
                                GraphQLCodeRegistry.newCodeRegistry()
                                        .defaultDataFetcher(new TeaQLDataFetcherFactory()))
                        .scalar(ExtendedScalars.GraphQLBigDecimal)
                        .scalar(ExtendedScalars.GraphQLLong)
                        .scalar(ExtendedScalars.Date)
                        .scalar(ExtendedScalars.Time)
                        .scalar(ExtendedScalars.LocalTime)
                        .scalar(ExtendedScalars.Json)
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
                                .graphQLContext(MapUtil.of("userContext", ctx))
                                .query(query));
        if (result.getErrors() != null && !result.getErrors().isEmpty()) {
            throw new TQLException(result.getErrors().toString());
        }
        return result.getData();
    }
}
