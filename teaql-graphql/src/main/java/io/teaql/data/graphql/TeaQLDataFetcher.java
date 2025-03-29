package io.teaql.data.graphql;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import cn.hutool.core.map.MapUtil;

import graphql.execution.DataFetcherResult;
import graphql.language.Field;
import graphql.language.Selection;
import graphql.language.SelectionSet;
import graphql.schema.DataFetcher;
import graphql.schema.DataFetcherFactoryEnvironment;
import graphql.schema.DataFetchingEnvironment;
import graphql.schema.GraphQLArgument;
import graphql.schema.GraphQLFieldDefinition;
import graphql.schema.GraphQLList;
import graphql.schema.GraphQLObjectType;
import graphql.schema.GraphQLType;
import graphql.schema.GraphQLTypeUtil;
import io.teaql.data.BaseEntity;
import io.teaql.data.BaseRequest;
import io.teaql.data.Entity;
import io.teaql.data.Repository;
import io.teaql.data.SearchRequest;
import io.teaql.data.SmartList;
import io.teaql.data.TQLException;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.Operator;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.Relation;

public class TeaQLDataFetcher implements DataFetcher {

    public static final long VIRTUAL_ROOT_ID = Long.MIN_VALUE;
    static final String DATA_CACHE_KEY_PREFIX = "GraphQL-Data:";
    private final DataFetcherFactoryEnvironment env;

    public TeaQLDataFetcher(DataFetcherFactoryEnvironment pEnv) {
        env = pEnv;
    }

    private static DataFetcherResult<Object> emptyResult() {
        return DataFetcherResult.newResult().data(null).build();
    }

    @Override
    public Object get(DataFetchingEnvironment environment) throws Exception {
        UserContext ctx = environment.getGraphQlContext().get("userContext");
        if (ctx == null) {
            throw new RuntimeException("No user context found");
        }
        // here will batch load this field value
        Map<Long, SmartList> fieldData = loadFieldData(ctx, environment);
        if (fieldData == null) {
            return emptyResult();
        }
        // virtual Root Id
        Long sourceId = VIRTUAL_ROOT_ID;
        if (!isRoot(environment)) {
            // the parent is null
            Entity source = environment.getSource();
            if (source == null) {
                return emptyResult();
            }
            if (relationKeptInParent(ctx, environment)) {
                sourceId = ((Entity) source.getProperty(getJoinPropertyName(ctx, environment))).getId();
            }
            else {
                sourceId = source.getId();
            }
        }
        return refineResult(environment, fieldData, sourceId);
    }

    private boolean relationKeptInParent(UserContext ctx, DataFetchingEnvironment environment) {
        String parentType = ((GraphQLObjectType) environment.getParentType()).getName();
        String parentRequestType = parentRequestType(environment);

        if (parentRequestType == null) {
            return false;
        }

        GraphQLSupport graphqlQueryFactory = ctx.getBean(GraphQLSupport.class);
        String queryPropertyName =
                graphqlQueryFactory.getRequestProperty(
                        new GraphQLFetcherParam(ctx, parentType, environment.getField().getName()));

        if (queryPropertyName == null) {
            return false;
        }

        Repository repository = ctx.resolveRepository(parentRequestType);
        EntityDescriptor entityDescriptor = repository.getEntityDescriptor();
        PropertyDescriptor property = entityDescriptor.findProperty(queryPropertyName);
        if (property == null) {
            throw new TQLException(
                    "Could not find property "
                            + queryPropertyName
                            + "in repository "
                            + entityDescriptor.getType());
        }

        if (property instanceof Relation r) {
            EntityDescriptor relationKeeper = r.getRelationKeeper();
            EntityDescriptor thisEntityDescriptor = entityDescriptor;
            while (thisEntityDescriptor != null) {
                if (thisEntityDescriptor == relationKeeper) {
                    return true;
                }
                thisEntityDescriptor = thisEntityDescriptor.getParent();
            }
            return false;
        }
        throw new TQLException(
                "property:" + queryPropertyName + " only relation supported, but now it's simple property");
    }

    private String getJoinPropertyName(UserContext ctx, DataFetchingEnvironment environment) {
        GraphQLSupport graphqlQueryFactory = ctx.getBean(GraphQLSupport.class);
        String parentType = ((GraphQLObjectType) environment.getParentType()).getName();
        return graphqlQueryFactory.getRequestProperty(
                new GraphQLFetcherParam(ctx, parentType, environment.getFieldDefinition().getName()));
    }

    private DataFetcherResult<Object> refineResult(
            DataFetchingEnvironment environment, Map<Long, SmartList> fieldData, Long sourceId) {
        SmartList dataList = fieldData.get(sourceId);
        if (dataList == null) {
            return emptyResult();
        }
        Entity first = dataList.first();
        if (first == null) {
            return emptyResult();
        }
        Map<Object, Object> localContext =
                MapUtil.builder()
                        .put("parentRequestType", first.typeName())
                        .put("parentPath", currentPath(environment))
                        .build();
        // inspect the return type
        if (GraphQLTypeUtil.isObjectType(env.getFieldDefinition().getType())) {
            // single value
            return DataFetcherResult.newResult().data(first).localContext(localContext).build();
        }
        else {
            // list value
            return DataFetcherResult.newResult()
                    .data(dataList.getData())
                    .localContext(localContext)
                    .build();
        }
    }

    private String currentPath(DataFetchingEnvironment env) {
        String parentPath = parentPath(env);
        return parentPath + "/" + env.getField().getName();
    }

    private String parentPath(DataFetchingEnvironment env) {
        String parentPath = "Query";
        Map<String, Object> localContext = env.getLocalContext();
        if (localContext != null) {
            String superPath = (String) localContext.get("parentPath");
            if (superPath != null) {
                parentPath = superPath;
            }
        }
        return parentPath;
    }

    protected boolean isRoot(DataFetchingEnvironment env) {
        GraphQLType parentType = env.getParentType();
        return parentType instanceof GraphQLObjectType obj && "Query".equalsIgnoreCase(obj.getName());
    }

    private Map<Long, SmartList> loadFieldData(UserContext ctx, DataFetchingEnvironment environment) {
        String path = currentPath(environment);
        String key = DATA_CACHE_KEY_PREFIX + path;

        Map<Long, SmartList> cache = (Map<Long, SmartList>) ctx.getObj(key);
        if (cache == null) {
            cache = new HashMap<>();
            ctx.put(key, cache);

            // batch load request
            SearchRequest request = getRequest(ctx, environment);
            SmartList result = request.executeForList(ctx);

            cache.put(VIRTUAL_ROOT_ID, result);
            if (isRoot(environment)) {
                return cache;
            }

            boolean relationKeptInParent = relationKeptInParent(ctx, environment);
            String relatedProperty = BaseEntity.ID_PROPERTY;
            if (!relationKeptInParent) {
                String parentRequestType = parentRequestType(environment);
                Repository repository = ctx.resolveRepository(parentRequestType);
                String parentType = ((GraphQLObjectType) environment.getParentType()).getName();
                GraphQLSupport graphqlQueryFactory = ctx.getBean(GraphQLSupport.class);
                String queryProperty =
                        graphqlQueryFactory.getRequestProperty(
                                new GraphQLFetcherParam(ctx, parentType, environment.getField().getName()));
                Relation relation = (Relation) repository.getEntityDescriptor().findProperty(queryProperty);
                relatedProperty = relation.getReverseProperty().getName();
            }

            // maintain relationship
            for (Object o : result) {
                Entity entity = (Entity) o;
                Entity parentValue = entity.getProperty(relatedProperty);
                if (parentValue != null) {
                    SmartList values = cache.get(parentValue.getId());
                    if (values == null) {
                        values = new SmartList();
                        cache.put(parentValue.getId(), values);
                    }
                    values.add(entity);
                }
            }
        }
        return cache;
    }

    protected SearchRequest getRequest(
            UserContext ctx, DataFetchingEnvironment dataFetchingEnvironment) {
        Field field = dataFetchingEnvironment.getField();
        GraphQLFieldDefinition fieldDefinition = env.getFieldDefinition();
        List<GraphQLArgument> arguments = fieldDefinition.getArguments();
        String parentType = ((GraphQLObjectType) dataFetchingEnvironment.getParentType()).getName();
        GraphQLSupport graphqlQueryFactory = ctx.getBean(GraphQLSupport.class);
        Object[] parameters = new Object[arguments.size()];
        for (int i = 0; i < arguments.size(); i++) {
            GraphQLArgument graphQLArgument = arguments.get(i);
            parameters[i] = dataFetchingEnvironment.getArgument(graphQLArgument.getName());
        }

        // call custom query builder to build the basic
        BaseRequest request =
                graphqlQueryFactory.buildQuery(
                        new GraphQLFetcherParam(ctx, parentType, field.getName(), parameters));

        // inspect output
        SelectionSet selectionSet = field.getSelectionSet();
        List<Selection> selections = selectionSet.getSelections();
        // output type
        String outputType = getOutputType(dataFetchingEnvironment.getFieldType());
        for (Selection selection : selections) {
            if (selection instanceof Field f) {
                String name = f.getName();
                String queryProperty =
                        graphqlQueryFactory.getRequestProperty(new GraphQLFetcherParam(ctx, outputType, name));
                PropertyDescriptor property =
                        ctx.resolveEntityDescriptor(outputType).findProperty(queryProperty);
                if (property != null && !(property instanceof Relation)) {
                    request.selectProperty(queryProperty);
                }
            }
        }

        // return the first item only, update the slice
        if (GraphQLTypeUtil.isObjectType(env.getFieldDefinition().getType())) {
            request.top(1);
        }

        // the request is ready for root fields
        if (isRoot(dataFetchingEnvironment)) {
            return request;
        }

        String parentRequestType = parentRequestType(dataFetchingEnvironment);
        String parentPath = parentPath(dataFetchingEnvironment);
        if (parentRequestType != null) {
            String parentKey = DATA_CACHE_KEY_PREFIX + parentPath;
            Map<Long, SmartList> cache = (Map<Long, SmartList>) ctx.getObj(parentKey);
            SmartList parentList = cache.get(VIRTUAL_ROOT_ID);

            String queryProperty =
                    graphqlQueryFactory.getRequestProperty(
                            new GraphQLFetcherParam(
                                    ctx, parentType, dataFetchingEnvironment.getField().getName()));
            // load upstream nodes
            if (relationKeptInParent(ctx, dataFetchingEnvironment)) {
                Object value =
                        parentList.stream()
                                .map(e -> ((Entity) e).getProperty(queryProperty))
                                .collect(Collectors.toSet());
                request.appendSearchCriteria(
                        request.createBasicSearchCriteria(BaseEntity.ID_PROPERTY, Operator.IN, value));
                request.selectProperty(BaseEntity.ID_PROPERTY);
            }
            else {
                // load downstream nodes
                Repository repository = ctx.resolveRepository(parentRequestType);
                Relation relation = (Relation) repository.getEntityDescriptor().findProperty(queryProperty);
                String name = relation.getReverseProperty().getName();
                request.appendSearchCriteria(
                        request.createBasicSearchCriteria(name, Operator.IN, parentList));
                request.selectProperty(name);
            }
        }
        return request;
    }

    private String getOutputType(GraphQLType fieldType) {
        if (fieldType instanceof GraphQLList l) {
            return getOutputType(l.getWrappedType());
        }
        if (fieldType instanceof GraphQLObjectType o) {
            return o.getName();
        }
        throw new TQLException("Unsupported field type: " + fieldType);
    }

    private String parentRequestType(DataFetchingEnvironment pDataFetchingEnvironment) {
        Map<String, Object> localContext = pDataFetchingEnvironment.getLocalContext();
        if (localContext != null) {
            String parentRequestType = (String) localContext.get("parentRequestType");
            return parentRequestType;
        }
        return null;
    }
}
