package io.teaql.data;

import cn.hutool.core.util.StrUtil;
import java.util.*;
import java.util.stream.Stream;

public interface SearchRequest<T extends Entity> {
  default String getTypeName() {
    String simpleName = this.getClass().getSimpleName();
    return StrUtil.removeSuffix(simpleName, "Request");
  }

  /**
   * raw sql for this search request when SQLRepository loadInternal
   *
   * @return custom provided full sql
   */
  default String getRawSql() {
    return null;
  }

  Class<? extends T> returnType();

  String comment();

  String getPartitionProperty();

  void setPartitionProperty(String propertyName);

  List<SimpleNamedExpression> getProjections();

  List<SimpleNamedExpression> getSimpleDynamicProperties();

  SearchCriteria getSearchCriteria();

  Aggregations getAggregations();

  Map<String, SearchRequest> getPropagateAggregations();

  Map<String, SearchRequest> getPropagateDimensions();

  OrderBys getOrderBy();

  Slice getSlice();

  Map<String, SearchRequest> enhanceRelations();

  Map<String, SearchRequest> enhanceChildren();

  List<SimpleAggregation> getDynamicAggregateAttributes();

  SearchRequest<T> appendSearchCriteria(SearchCriteria searchCriteria);

  default T execute(UserContext userContext) {
    if (userContext == null) {
      throw new RepositoryException("userContext is null");
    }
    return userContext.execute(this);
  }

  default SmartList<T> executeForList(UserContext userContext) {
    if (userContext == null) {
      throw new RepositoryException("userContext is null");
    }
    return userContext.executeForList(this);
  }

  default Stream<T> executeForStream(UserContext userContext) {
    if (userContext == null) {
      throw new RepositoryException("userContext is null");
    }
    return userContext.executeForStream(this);
  }

  default Stream<T> executeForStream(UserContext userContext, int enhanceBatchSize) {
    if (userContext == null) {
      throw new RepositoryException("userContext is null");
    }
    return userContext.executeForStream(this, enhanceBatchSize);
  }

  default AggregationResult aggregation(UserContext userContext) {
    if (userContext == null) {
      throw new RepositoryException("userContext is null");
    }
    return userContext.aggregation(this);
  }

  default boolean hasSimpleAgg() {
    Aggregations aggregations = getAggregations();
    if (aggregations == null) {
      return false;
    }
    return !aggregations.getAggregates().isEmpty();
  }

  default List<String> dataProperties(UserContext ctx) {
    Set<String> allRelationProperties = new HashSet<>();
    List<SimpleNamedExpression> projections = getProjections();
    if (projections != null) {
      for (SimpleNamedExpression projection : projections) {
        allRelationProperties.addAll(projection.properties(ctx));
      }
    }

    List<SimpleNamedExpression> simpleDynamicProperties = getSimpleDynamicProperties();
    if (simpleDynamicProperties != null) {
      for (SimpleNamedExpression dynamicProperty : simpleDynamicProperties) {
        allRelationProperties.addAll(dynamicProperty.properties(ctx));
      }
    }

    SearchCriteria searchCriteria = getSearchCriteria();
    if (searchCriteria != null) {
      allRelationProperties.addAll(searchCriteria.properties(ctx));
    }

    String partitionProperty = getPartitionProperty();
    if (partitionProperty != null && getSlice().getSize() != 0) {
      allRelationProperties.add(partitionProperty);
    }

    OrderBys orderBy = getOrderBy();
    if (orderBy != null) {
      allRelationProperties.addAll(orderBy.properties(ctx));
    }

    return new ArrayList<>(allRelationProperties);
  }

  default List<String> aggregationProperties(UserContext ctx) {
    Set<String> allRelationProperties = new HashSet<>();
    List<SimpleNamedExpression> all = getAggregations().getSelectedExpressions();
    for (SimpleNamedExpression simpleNamedExpression : all) {
      allRelationProperties.addAll(simpleNamedExpression.properties(ctx));
    }
    SearchCriteria searchCriteria = getSearchCriteria();
    if (searchCriteria != null) {
      allRelationProperties.addAll(searchCriteria.properties(ctx));
    }
    return new ArrayList<>(allRelationProperties);
  }

  default boolean tryUseSubQuery() {
    return true;
  }

  default boolean tryCacheAggregation() {
    return false;
  }

  default long getAggregateCacheTime() {
    return 0l;
  }
}
