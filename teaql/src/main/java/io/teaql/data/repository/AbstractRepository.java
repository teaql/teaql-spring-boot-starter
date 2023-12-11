package io.teaql.data.repository;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.comparator.CompareUtil;
import cn.hutool.core.util.NumberUtil;
import cn.hutool.core.util.ObjectUtil;
import io.teaql.data.*;
import io.teaql.data.criteria.*;
import io.teaql.data.event.EntityCreatedEvent;
import io.teaql.data.event.EntityDeletedEvent;
import io.teaql.data.event.EntityRecoverEvent;
import io.teaql.data.event.EntityUpdatedEvent;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.Relation;
import java.util.*;
import java.util.stream.Collectors;

public abstract class AbstractRepository<T extends Entity> implements Repository<T> {

  public static final String VERSION = "version";
  public static final String ID = "id";

  protected abstract void updateInternal(UserContext ctx, Collection<T> items);

  protected abstract void createInternal(UserContext ctx, Collection<T> items);

  protected abstract void deleteInternal(UserContext userContext, Collection<T> deleteItems);

  protected abstract void recoverInternal(UserContext userContext, Collection<T> recoverItems);

  protected abstract SmartList<T> loadInternal(UserContext userContext, SearchRequest<T> request);

  protected abstract AggregationResult aggregateInternal(
      UserContext userContext, SearchRequest<T> request);

  @Override
  public Collection<T> save(UserContext userContext, Collection<T> entities) {
    if (ObjectUtil.isEmpty(entities)) {
      return entities;
    }
    Collection<T> newItems = CollUtil.filterNew(entities, Entity::newItem);
    if (ObjectUtil.isNotEmpty(newItems)) {
      for (T newItem : newItems) {
        setIdAndVersionForInsert(userContext, newItem);
      }
      createInternal(userContext, newItems);
      for (T newItem : newItems) {
        if (newItem instanceof BaseEntity item) {
          item.gotoNextStatus(EntityAction.PERSIST);
          userContext.sendEvent(new EntityCreatedEvent(item));
          userContext.afterPersist(item);
        }
      }
    }
    Collection<T> updatedItems = CollUtil.filterNew(entities, Entity::updateItem);
    if (ObjectUtil.isNotEmpty(updatedItems)) {
      updateInternal(userContext, updatedItems);
      for (T updateItem : updatedItems) {
        updateItem.setVersion(updateItem.getVersion() + 1);
        if (updateItem instanceof BaseEntity item) {
          userContext.sendEvent(new EntityUpdatedEvent(item));
          item.gotoNextStatus(EntityAction.PERSIST);
          userContext.afterPersist(item);
        }
      }
    }
    Collection<T> deleteItems = CollUtil.filterNew(entities, Entity::deleteItem);
    if (ObjectUtil.isNotEmpty(deleteItems)) {
      deleteInternal(userContext, deleteItems);
      for (T deleteItem : deleteItems) {
        deleteItem.setVersion(-(deleteItem.getVersion() + 1));
        if (deleteItem instanceof BaseEntity item) {
          item.gotoNextStatus(EntityAction.PERSIST);
          userContext.sendEvent(new EntityDeletedEvent(item));
          userContext.afterPersist(item);
        }
      }
    }

    Collection<T> recoverItems = CollUtil.filterNew(entities, Entity::recoverItem);
    if (ObjectUtil.isNotEmpty(recoverItems)) {
      recoverInternal(userContext, recoverItems);
      for (T recoverItem : recoverItems) {
        recoverItem.setVersion(-recoverItem.getVersion() + 1);
        if (recoverItem instanceof BaseEntity item) {
          item.gotoNextStatus(EntityAction.PERSIST);
          userContext.sendEvent(new EntityRecoverEvent(item));
          userContext.afterPersist(item);
        }
      }
    }
    return entities;
  }

  private void setIdAndVersionForInsert(UserContext userContext, Entity entity) {
    Long id = prepareId(userContext, (T) entity);
    entity.setId(id);
    entity.setVersion(1L);
  }

  /**
   * check if current relation is handled by this repository
   *
   * @param relation relation
   * @return true if current relation is handled(save/query) by this repository
   */
  public boolean shouldHandle(Relation relation) {
    if (relation == null) {
      throw new IllegalArgumentException("relation is null");
    }
    EntityDescriptor relationKeeper = relation.getRelationKeeper();
    EntityDescriptor entityDescriptor = getEntityDescriptor();
    while (entityDescriptor != null) {
      if (entityDescriptor == relationKeeper) {
        return true;
      }
      entityDescriptor = entityDescriptor.getParent();
    }
    return false;
  }

  @Override
  public SmartList<T> executeForList(UserContext userContext, SearchRequest<T> request) {
    SmartList<T> smartList = loadInternal(userContext, request);
    enhanceChildren(userContext, smartList, request);
    enhanceRelations(userContext, smartList, request);
    enhanceWithAggregation(userContext, smartList, request);
    addDynamicAggregations(userContext, smartList, request);
    return smartList;
  }

  private void enhanceChildren(
      UserContext userContext, SmartList<T> dataSet, SearchRequest<T> request) {
    if (dataSet == null || dataSet.isEmpty()) {
      return;
    }
    Map<String, SearchRequest> childrenRequest = request.enhanceChildren();
    if (ObjectUtil.isEmpty(childrenRequest)) {
      return;
    }
    Map<Long, Integer> itemLocation = new HashMap<>();
    int i = 0;
    for (T t : dataSet) {
      itemLocation.put(t.getId(), i++);
    }
    childrenRequest.forEach(
        (type, childRequest) -> {
          TempRequest tempRequest = new TempRequest(childRequest);
          tempRequest.appendSearchCriteria(
              tempRequest.createBasicSearchCriteria(BaseEntity.ID_PROPERTY, Operator.IN, dataSet));
          SmartList childrenItems = tempRequest.executeForList(userContext);
          for (Object item : childrenItems) {
            T subItem = (T) item;
            Long id = subItem.getId();
            Integer location = itemLocation.get(id);
            T oldItem = dataSet.get(location);
            copyProperties(subItem, oldItem);
            dataSet.set(location, subItem);
          }
        });
  }

  protected void copyProperties(T subItem, T parentItem) {
    EntityDescriptor entityDescriptor = getEntityDescriptor();
    while (entityDescriptor != null) {
      List<PropertyDescriptor> properties = entityDescriptor.getProperties();
      for (PropertyDescriptor property : properties) {
        String name = property.getName();
        subItem.setProperty(name, parentItem.getProperty(name));
      }
      entityDescriptor = entityDescriptor.getParent();
    }
  }

  protected void enhanceWithAggregation(
      UserContext userContext, SmartList<T> dataSet, SearchRequest<T> request) {
    List<SearchRequest> aggregationRequests = findAggregations(userContext, request);
    for (SearchRequest aggregationRequest : aggregationRequests) {
      AggregationResult aggregation = aggregationRequest.aggregation(userContext);
      dataSet.addAggregationResult(userContext, aggregation);
    }
  }

  public void enhanceRelations(
      UserContext userContext, SmartList<T> dataSet, SearchRequest<T> request) {
    if (dataSet == null || dataSet.isEmpty()) {
      return;
    }
    Map<String, SearchRequest> enhanceProperties = request.enhanceRelations();
    enhanceProperties.forEach(
        (p, r) -> {
          PropertyDescriptor property = findProperty(p);
          if (property == null) {
            return;
          }

          if (!(property instanceof Relation)) {
            return;
          }

          if (shouldHandle((Relation) property)) {
            enhanceParent(userContext, dataSet, (Relation) property, r);
          } else {
            collectChildren(userContext, dataSet, (Relation) property, r);
          }
        });
  }

  private void enhanceParent(
      UserContext userContext,
      SmartList<T> results,
      Relation relation,
      SearchRequest parentRequest) {
    if (ObjectUtil.isEmpty(results)) {
      return;
    }
    List<Entity> parents =
        results.stream()
            .map(e -> e.getProperty(relation.getName()))
            .filter(p -> p instanceof Entity)
            .map(e -> (Entity) e)
            .distinct()
            .toList();
    if (ObjectUtil.isEmpty(parents)) {
      return;
    }

    // parent request add id criteria
    TempRequest parentTemp = new TempRequest(parentRequest);
    parentTemp.appendSearchCriteria(parentTemp.createBasicSearchCriteria(ID, Operator.IN, parents));
    Repository repository = userContext.resolveRepository(parentTemp.getTypeName());
    SmartList parentItems = repository.executeForList(userContext, parentTemp);

    Map map = parentItems.mapById();
    for (T result : results) {
      Object oldValue = result.getProperty(relation.getName());
      if (oldValue instanceof Entity) {
        result.addRelation(relation.getName(), (Entity) map.get(((Entity) oldValue).getId()));
      }
    }
  }

  private void collectChildren(
      UserContext userContext,
      SmartList<T> dataSet,
      Relation relation,
      SearchRequest childRequest) {
    if (dataSet == null || dataSet.isEmpty()) {
      return;
    }
    TempRequest childTempRequest = new TempRequest(childRequest);
    String typeName = childTempRequest.getTypeName();
    Repository repository = userContext.resolveRepository(typeName);
    PropertyDescriptor reverseProperty = relation.getReverseProperty();
    if (childTempRequest.getSlice() != null) {
      childTempRequest.setPartitionProperty(reverseProperty.getName());
    }
    childTempRequest.appendSearchCriteria(
        childTempRequest.createBasicSearchCriteria(
            reverseProperty.getName(), Operator.IN, dataSet));
    SmartList children = repository.executeForList(userContext, childTempRequest);

    Map<Long, T> longTMap = dataSet.mapById();
    for (Object child : children) {
      Entity childEntity = (Entity) child;
      Object parent = childEntity.getProperty(reverseProperty.getName());
      if (parent instanceof Entity) {
        T parentEntity = longTMap.get(((Entity) parent).getId());
        if (parentEntity != null) {
          parentEntity.addRelation(relation.getName(), childEntity);
        }
      }
    }
  }

  public PropertyDescriptor findProperty(String propertyName) {
    EntityDescriptor entityDescriptor = getEntityDescriptor();
    while (entityDescriptor != null) {
      PropertyDescriptor propertyDescriptor = entityDescriptor.findProperty(propertyName);
      if (propertyDescriptor != null) {
        return propertyDescriptor;
      }
      entityDescriptor = entityDescriptor.getParent();
    }
    throw new RepositoryException("Property: " + propertyName + " not defined");
  }

  public void addDynamicAggregations(
      UserContext userContext, SmartList<T> dataSet, SearchRequest<T> request) {
    if (dataSet == null || dataSet.isEmpty()) {
      return;
    }

    List<SimpleAggregation> dynamicAggregateAttributes = request.getDynamicAggregateAttributes();
    if (ObjectUtil.isEmpty(dynamicAggregateAttributes)) {
      return;
    }

    Map<Long, T> idEntityMap = dataSet.mapById();
    Set<Long> ids = idEntityMap.keySet();
    if (ObjectUtil.isEmpty(ids)) {
      return;
    }

    for (SimpleAggregation dynamicAggregateAttribute : dynamicAggregateAttributes) {
      SearchRequest aggregateRequest = dynamicAggregateAttribute.getAggregateRequest();
      String property = aggregateRequest.getPartitionProperty();
      TempRequest t = new TempRequest(aggregateRequest);
      t.groupBy(property);
      if (ids.size() < preferIdInCount()) {
        t.appendSearchCriteria(t.createBasicSearchCriteria(property, Operator.IN, ids));
      } else {
        t.appendSearchCriteria(new SubQuerySearchCriteria(property, request, ID));
      }
      List<SearchRequest> aggregations = findAggregations(userContext, t);
      SearchRequest aggregatePoint = aggregations.get(0);
      AggregationResult aggregation = aggregatePoint.aggregation(userContext);
      if (dynamicAggregateAttribute.isSingleNumber()) {
        saveSingleDynamicValue(idEntityMap, dynamicAggregateAttribute, aggregation);
      } else {
        List<Map<String, Object>> dynamicAttributes = aggregation.toList();
        for (Map<String, Object> dynamicAttribute : dynamicAttributes) {
          saveMultiDynamicValue(idEntityMap, dynamicAggregateAttribute, property, dynamicAttribute);
        }
      }
    }
  }

  protected int preferIdInCount() {
    return 1000;
  }

  public List<SearchRequest> findAggregations(UserContext userContext, SearchRequest request) {
    List<SearchRequest> ret = new ArrayList<>();
    if (request.hasSimpleAgg()) {
      ret.add(request);
    }
    Map<String, SearchRequest> propagateAggregations = request.getPropagateAggregations();
    propagateAggregations.forEach(
        (property, subRequest) -> {
          TempRequest t = new TempRequest(subRequest);
          PropertyDescriptor propertyDescriptor = findProperty(property);
          if (shouldHandle((Relation) propertyDescriptor)) {
            t.appendSearchCriteria(new SubQuerySearchCriteria(ID, request, property));
          } else {
            PropertyDescriptor reverseProperty =
                ((Relation) propertyDescriptor).getReverseProperty();
            t.appendSearchCriteria(
                new SubQuerySearchCriteria(reverseProperty.getName(), request, ID));
          }
          List<SearchRequest> aggregations = findAggregations(userContext, t);
          if (aggregations != null) {
            ret.addAll(aggregations);
          }
        });
    return ret;
  }

  private void saveMultiDynamicValue(
      Map<Long, T> idEntityMap,
      SimpleAggregation dynamicAggregateAttribute,
      String property,
      Map<String, Object> dynamicAttribute) {
    Long parentID = ((Number) dynamicAttribute.remove(property)).longValue();
    T parent = idEntityMap.get(parentID);
    parent.appendDynamicProperty(dynamicAggregateAttribute.getName(), dynamicAttribute);
  }

  private void saveSingleDynamicValue(
      Map<Long, T> idEntityMap,
      SimpleAggregation dynamicAggregateAttribute,
      AggregationResult aggregation) {
    Map<Object, Number> simpleMap = aggregation.toSimpleMap();
    simpleMap.forEach(
        (parentId, value) -> {
          if(parentId instanceof Number numberParentId){
            T parent = idEntityMap.get(numberParentId.longValue());
            parent.addDynamicProperty(dynamicAggregateAttribute.getName(), value);
            return;
          }
          T parent = idEntityMap.get(parentId);
          if(parent==null){
            throw new IllegalArgumentException("Not able to find parent object from idEntityMap by key: " +parentId +", with class" +parentId.getClass().getSimpleName() )
          }
          parent.addDynamicProperty(dynamicAggregateAttribute.getName(), value);
          
        });
  }

  public void advanceGroupBy(
      UserContext userContext, AggregationResult result, SearchRequest<T> request) {
    Map<String, SearchRequest> propagateDimensions = request.getPropagateDimensions();
    List<SimpleNamedExpression> allDimensions = request.getAggregations().getDimensions();
    propagateDimensions.forEach(
        (property, dimensionRequest) -> {
          SimpleNamedExpression toBeEnhancedDimension =
              findCurrentDimension(allDimensions, property);

          List propagateDimensionValues = result.getPropagateDimensionValues(property);
          TempRequest t =
              new TempRequest(dimensionRequest.returnType(), dimensionRequest.getTypeName());

          t.appendSearchCriteria(dimensionRequest.getSearchCriteria());
          t.addSimpleDynamicProperty(property, new PropertyReference(ID));

          Map<String, SearchRequest> subPropagateDimensions =
              dimensionRequest.getPropagateDimensions();
          subPropagateDimensions.forEach(
              (k, v) -> {
                t.groupBy(k, v);
              });

          List<SimpleNamedExpression> dimensions =
              dimensionRequest.getAggregations().getDimensions();
          for (SimpleNamedExpression dimension : dimensions) {
            t.addSimpleDynamicProperty(dimension.name(), dimension.getExpression());
          }
          t.appendSearchCriteria(
              t.createBasicSearchCriteria(ID, Operator.IN, propagateDimensionValues));
          SmartList orderByResults = t.executeForList(userContext);
          appendResult(userContext, result, t, toBeEnhancedDimension, orderByResults);
        });
  }

  private SimpleNamedExpression findCurrentDimension(
      List<SimpleNamedExpression> allDimensions, String property) {
    for (SimpleNamedExpression dimension : allDimensions) {
      if (dimension.name().equals(property)) {
        return dimension;
      }
    }
    return null;
  }

  private void appendResult(
      UserContext userContext,
      AggregationResult result,
      SearchRequest dimensionRequest,
      SimpleNamedExpression toBeRefinedDimension,
      SmartList<Entity> dimensionResult) {
    List<SimpleNamedExpression> simpleDynamicProperties =
        dimensionRequest.getSimpleDynamicProperties();

    Map<Object, Map<SimpleNamedExpression, Object>> refinedDimensions =
        CollStreamUtil.toMap(
            dimensionResult.getData(),
            e -> e.getDynamicProperty(toBeRefinedDimension.name()),
            e -> {
              Map<SimpleNamedExpression, Object> refinedDimension = new HashMap<>();
              for (SimpleNamedExpression simpleDynamicProperty : simpleDynamicProperties) {
                if (simpleDynamicProperty.name().equals(toBeRefinedDimension.name())) {
                  continue;
                }
                refinedDimension.put(
                    simpleDynamicProperty, e.getDynamicProperty(simpleDynamicProperty.name()));
              }
              return refinedDimension;
            });

    List<AggregationItem> data = result.getData();

    for (AggregationItem datum : data) {
      Map<SimpleNamedExpression, Object> dimensions = datum.getDimensions();
      Object currentValue = remove(dimensions, toBeRefinedDimension);
      if (currentValue == null) {
        continue;
      }
      Map<SimpleNamedExpression, Object> replacements = refinedDimensions.get(currentValue);
      if (replacements != null) {
        dimensions.putAll(replacements);
      }
    }

    // merge
    Map<Map<SimpleNamedExpression, Object>, AggregationItem> collect =
        data.stream()
            .collect(
                Collectors.toMap(
                    item -> item.getDimensions(),
                    item -> item,
                    (pre, current) -> {
                      Map<SimpleNamedExpression, Object> preValues = pre.getValues();
                      Map<SimpleNamedExpression, Object> currentValues = current.getValues();
                      Set<SimpleNamedExpression> simpleNamedExpressions = preValues.keySet();
                      for (SimpleNamedExpression simpleNamedExpression : simpleNamedExpressions) {
                        preValues.put(
                            simpleNamedExpression,
                            merge(
                                simpleNamedExpression,
                                preValues.get(simpleNamedExpression),
                                currentValues.get(simpleNamedExpression)));
                      }
                      return pre;
                    }));

    advanceGroupBy(userContext, result, dimensionRequest);
  }

  private Object remove(
      Map<SimpleNamedExpression, Object> dimensions, SimpleNamedExpression toBeRefinedDimension) {
    Set<Map.Entry<SimpleNamedExpression, Object>> entries = dimensions.entrySet();
    Iterator<Map.Entry<SimpleNamedExpression, Object>> iterator = entries.iterator();
    while (iterator.hasNext()) {
      Map.Entry<SimpleNamedExpression, Object> next = iterator.next();
      SimpleNamedExpression key = next.getKey();
      Object value = next.getValue();
      if (key.name().equals(toBeRefinedDimension.name())) {
        iterator.remove();
        return value;
      }
    }
    return null;
  }

  private Object merge(SimpleNamedExpression aggregation, Object p0, Object p1) {
    Expression expression = aggregation.getExpression();
    if (!(expression instanceof FunctionApply)) {
      throw new RepositoryException("FunctionApply expression only for aggregation");
    }

    PropertyFunction operator = ((FunctionApply) expression).getOperator();
    if (!(operator instanceof AggrFunction)) {
      throw new RepositoryException("Operator expression only for aggregation");
    }
    AggrFunction aggr = (AggrFunction) operator;
    if (aggr == AggrFunction.COUNT || aggr == AggrFunction.SUM) {
      return NumberUtil.add((Number) p0, (Number) p1);
    }

    if (aggr == AggrFunction.MIN) {
      return CompareUtil.compare((Comparable) p0, (Comparable) p1) < 0 ? p0 : p1;
    }

    if (aggr == AggrFunction.MAX) {
      return CompareUtil.compare((Comparable) p0, (Comparable) p1) < 0 ? p1 : p0;
    }

    throw new RepositoryException("unsupported AggrFunction" + aggr);
  }

  @Override
  public AggregationResult aggregation(UserContext userContext, SearchRequest<T> request) {
    AggregationResult result = aggregateInternal(userContext, request);
    if (result == null) {
      return null;
    }
    advanceGroupBy(userContext, result, request);
    return result;
  }
}
