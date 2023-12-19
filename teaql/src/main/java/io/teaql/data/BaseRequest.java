package io.teaql.data;

import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import com.fasterxml.jackson.databind.JsonNode;
import io.teaql.data.criteria.*;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.Relation;
import java.util.*;
import java.util.stream.Collectors;

public abstract class BaseRequest<T extends Entity> implements SearchRequest<T> {

  public static final String REFINEMENTS = "refinements";

  // select properties
  List<SimpleNamedExpression> projections = new ArrayList<>();

  // simple dynamic properties
  List<SimpleNamedExpression> simpleDynamicProperties = new ArrayList<>();

  // search conditions
  SearchCriteria searchCriteria;

  // order by
  OrderBys orderBys = new OrderBys();

  // paging
  Slice slice = new Slice();

  // enhance relations
  Map<String, SearchRequest> enhanceRelations = new HashMap<>();

  // 动态属性
  List<SimpleAggregation> dynamicAggregateAttributes = new ArrayList<>();

  // enhance lists and partition by parent
  String partitionProperty;

  // basic return type
  Class<? extends T> returnType;

  // aggregations
  Aggregations aggregations = new Aggregations();
  Map<String, SearchRequest> propagateAggregations = new HashMap<>();

  // group by, with aggregations
  Map<String, SearchRequest> propagateDimensions = new HashMap<>();

  Map<String, SearchRequest> enhanceChildren = new HashMap<>();

  public BaseRequest(Class<T> pReturnType) {
    returnType = pReturnType;
  }

  protected void setReturnType(Class<? extends T> pReturnType) {
    returnType = pReturnType;
  }

  @Override
  public Class<? extends T> returnType() {
    return returnType;
  }

  // 尝试load 对象本身（存储自身的所有的表）
  public BaseRequest<T> selectSelf() {
    return this;
  }

  // 尝试load 对象本身（存储自身的所有的表）,以及引用的对象的self，以及1对1关系的self
  public BaseRequest<T> selectAll() {
    return this;
  }

  // 尝试load 对象本身（存储自身的所有的表）,以及引用的对象的self，以及所有关系的self
  public BaseRequest<T> selectAny() {
    return this;
  }

  public void selectProperty(String propertyName) {
    if (ObjectUtil.isEmpty(propertyName)) {
      return;
    }
    for (SimpleNamedExpression projection : this.projections) {
      if (projection.name().equals(propertyName)) {
        return;
      }
    }
    this.projections.add(new SimpleNamedExpression(propertyName));
  }

  public void unselectProperty(String propertyName) {
    if (ObjectUtil.isEmpty(propertyName)) {
      return;
    }
    this.projections.removeIf(p -> p.name().equals(propertyName));
    this.enhanceRelations.remove(propertyName);
  }

  public void enhanceRelation(String propertyName, SearchRequest request) {
    this.enhanceRelations.put(propertyName, request);
  }

  @Override
  public List<SimpleNamedExpression> getProjections() {
    return projections;
  }

  @Override
  public SearchCriteria getSearchCriteria() {
    return searchCriteria;
  }

  @Override
  public OrderBys getOrderBy() {
    return orderBys;
  }

  @Override
  public Slice getSlice() {
    return slice;
  }

  @Override
  public Map<String, SearchRequest> enhanceRelations() {
    return enhanceRelations;
  }

  @Override
  public BaseRequest<T> appendSearchCriteria(SearchCriteria searchCriteria) {
    if (searchCriteria == null) {
      return this;
    }
    if (this.searchCriteria == null) {
      this.searchCriteria = searchCriteria;
    } else if (this.searchCriteria instanceof AND) {
      ((AND) this.searchCriteria).getExpressions().add(searchCriteria);
    } else {
      this.searchCriteria = SearchCriteria.and(this.searchCriteria, searchCriteria);
    }
    return this;
  }

  protected List<Expression> extractSearchCriteriaExcludeVersion(BaseRequest<T> anotherRequest) {
    AND andSearchCriteria = (AND) anotherRequest.getSearchCriteria();
    List<Expression> subExpression =
        andSearchCriteria.getExpressions().stream()
            .filter(expression -> expression instanceof SearchCriteria)
            .filter(expression -> !(expression instanceof VersionSearchCriteria))
            .collect(Collectors.toList());
    return subExpression;
  }

  protected BaseRequest<T> buildRequest(Map<String, Object> map) {
    String typeName = getTypeName();
    BaseRequest newReq = new TempRequest(this.returnType, typeName);
    map.entrySet()
        .forEach(
            stringObjectEntry -> {
              if (!stringObjectEntry.getKey().contains(".")) {
                // newReq.appendSearchCriteria(createBasicSearchCriteria())
              }
            });
    return newReq;
  }

  protected BaseRequest<T> internalMatchAny(BaseRequest<T> anotherRequest) {
    if (searchCriteria == null) {
      return this;
    }

    if (anotherRequest.getSearchCriteria() == null) {
      return this;
    }

    if (anotherRequest.getSearchCriteria() instanceof VersionSearchCriteria) {
      return this;
    }

    if (!(anotherRequest.getSearchCriteria() instanceof AND)) {
      this.appendSearchCriteria(anotherRequest.getSearchCriteria());
      return this;
    }

    List<Expression> subExpress = extractSearchCriteriaExcludeVersion(anotherRequest);
    // collection search criteria other than version criteria
    int length = subExpress.size();
    SearchCriteria[] searchCriteriaArray = new SearchCriteria[length];
    for (int i = 0; i < length; i++) {
      searchCriteriaArray[i] = (SearchCriteria) subExpress.get(i);
    }
    ((AND) this.searchCriteria).getExpressions().add(SearchCriteria.or(searchCriteriaArray));
    return this;
  }

  protected BaseRequest<T> withDeletedRows() {
    removeTopVersionCriteria();
    return this;
  }

  protected BaseRequest<T> deletedRowsOnly() {
    removeTopVersionCriteria();
    appendSearchCriteria(
        createBasicSearchCriteria(BaseEntity.VERSION_PROPERTY, Operator.LESS_THAN, 0l));
    return this;
  }

  protected void removeTopVersionCriteria() {
    SearchCriteria searchCriteria = getSearchCriteria();
    if (searchCriteria == null) {
      return;
    }

    if (searchCriteria instanceof VersionSearchCriteria) {
      this.searchCriteria = null;
    }

    if (!(searchCriteria instanceof AND)) {
      return;
    }

    List<Expression> expressions = ((AND) searchCriteria).getExpressions();
    Iterator<Expression> iterator = expressions.iterator();
    while (iterator.hasNext()) {
      Expression next = iterator.next();
      if (next instanceof VersionSearchCriteria) {
        iterator.remove();
      }
    }
  }

  public BaseRequest<T> top(int topN) {
    this.slice = new Slice();
    this.slice.setSize(topN);
    return this;
  }

  public BaseRequest<T> offset(int offset, int size) {
    this.slice = new Slice();
    this.slice.setOffset(offset);
    this.slice.setSize(size);
    return this;
  }

  public void addOrderByAscending(String propertyName) {
    orderBys.addOrderBy(new OrderBy(propertyName));
  }

  public void addOrderByDescending(String propertyName) {
    orderBys.addOrderBy(new OrderBy(propertyName, "DESC"));
  }

  public void addOrderByAscendingUsingGBK(String propertyName) {
    orderBys.addOrderBy(new OrderBy(AggrFunction.GBK, propertyName, "ASC"));
  }

  public void addOrderByDescendingUsingGBK(String propertyName) {
    orderBys.addOrderBy(new OrderBy(AggrFunction.GBK, propertyName, "DESC"));
  }

  @Override
  public String getPartitionProperty() {
    return partitionProperty;
  }

  @Override
  public void setPartitionProperty(String pPartitionProperty) {
    partitionProperty = pPartitionProperty;
  }

  @Override
  public Aggregations getAggregations() {
    return aggregations;
  }

  public void setAggregations(Aggregations pAggregations) {
    aggregations = pAggregations;
  }

  public Map<String, SearchRequest> getPropagateAggregations() {
    return propagateAggregations;
  }

  public void setPropagateAggregations(Map<String, SearchRequest> pPropagateAggregations) {
    propagateAggregations = pPropagateAggregations;
  }

  public Map<String, SearchRequest> getPropagateDimensions() {
    return propagateDimensions;
  }

  public void setPropagateDimensions(Map<String, SearchRequest> pPropagateDimensions) {
    propagateDimensions = pPropagateDimensions;
  }

  protected void internalFindWithJson(JsonNode jsonNode) {

    DynamicSearchHelper helper = new DynamicSearchHelper();
    helper.mergeClauses(this, jsonNode);
  }

  protected void internalFindWithJsonExpr(String jsonNodeExpr) {
    internalFindWithJson(DynamicSearchHelper.jsonFromString(jsonNodeExpr));
  }

  @Override
  public List<SimpleNamedExpression> getSimpleDynamicProperties() {
    return simpleDynamicProperties;
  }

  public void addSimpleDynamicProperty(String name, Expression expression) {
    this.simpleDynamicProperties.add(new SimpleNamedExpression(name, expression));
  }

  public void addAggregateDynamicProperty(
      String name, SearchRequest subRequest, boolean singleValueResult) {
    this.dynamicAggregateAttributes.add(new SimpleAggregation(name, subRequest, singleValueResult));
  }

  public void addAggregateDynamicProperty(String name, SearchRequest subRequest) {
    this.addAggregateDynamicProperty(name, subRequest, false);
  }

  public void addSingleAggregateDynamicProperty(String name, SearchRequest subRequest) {
    this.addAggregateDynamicProperty(name, subRequest, true);
  }

  public SearchCriteria createBasicSearchCriteria(
      String property, Operator operator, Object... values) {
    operator = refineOperator(operator, values);
    SearchCriteria searchCriteria = internalCreateSearchCriteria(property, operator, values);
    if (searchCriteria != null) {
      if ("version".equals(property)) {
        searchCriteria = new VersionSearchCriteria(searchCriteria);
      }
      return searchCriteria;
    }
    throw new RepositoryException("不支持的operator:" + operator);
  }

  private SearchCriteria internalCreateSearchCriteria(
      String property, Operator operator, Object[] values) {
    if (operator == Operator.SOUNDS_LIKE) {
      return new EQ(
          new FunctionApply(Operator.SOUNDS_LIKE, new PropertyReference(property)),
          new FunctionApply(Operator.SOUNDS_LIKE, new Parameter(property, values, operator)));
    }
    if (operator.hasOneOperator()) {
      return new OneOperatorCriteria(operator, new PropertyReference(property));
    } else if (operator.hasTwoOperator()) {
      return new TwoOperatorCriteria(
          operator, new PropertyReference(property), new Parameter(property, values, operator));
    } else if (operator.isBetween()) {
      if (ArrayUtil.length(values) != 2) {
        throw new RepositoryException("Between需要下限和上限两个参数");
      }
      return new Between(
          new PropertyReference(property),
          new Parameter(property, values[0], operator),
          new Parameter(property, values[1], operator));
    }
    return null;
  }

  public Operator refineOperator(Operator pOperator, Object value) {
    int itemCount = ObjectUtil.length(Parameter.flatValues(value));
    boolean tooManyItem = itemCount > 20;
    boolean multiValue = itemCount > 1;
    switch (pOperator) {
      case EQUAL:
      case IN:
      case IN_LARGE:
        if (tooManyItem) {
          return Operator.IN_LARGE;
        } else if (multiValue) {
          return Operator.IN;
        } else {
          return Operator.EQUAL;
        }
      case NOT_IN_LARGE:
      case NOT_EQUAL:
      case NOT_IN:
        if (tooManyItem) {
          return Operator.NOT_IN_LARGE;
        } else if (multiValue) {
          return Operator.NOT_IN;
        } else {
          return Operator.NOT_EQUAL;
        }
    }
    return pOperator;
  }

  public void addAggregate(SimpleNamedExpression aggregate) {
    List<SimpleNamedExpression> aggregates = getAggregations().getAggregates();
    String aggregateName = aggregate.name();
    // 对于重复添加同一名称的聚合，忽略掉它
    for (SimpleNamedExpression simpleNamedExpression : aggregates) {
      String name = simpleNamedExpression.name();
      if (aggregateName.equals(name)) {
        return;
      }
    }
    aggregates.add(aggregate);
  }

  public void aggregate(String property, SearchRequest subRequest) {
    this.propagateAggregations.put(property, subRequest);
  }

  public List<SimpleAggregation> getDynamicAggregateAttributes() {
    return dynamicAggregateAttributes;
  }

  public void setDynamicAggregateAttributes(List<SimpleAggregation> pDynamicAggregateAttributes) {
    dynamicAggregateAttributes = pDynamicAggregateAttributes;
  }

  public void groupBy(String propertyName) {
    groupBy(propertyName, propertyName);
  }

  public void groupBy(String retName, String propertyName) {
    groupBy(retName, propertyName, AggrFunction.SELF);
  }

  public void groupBy(String retName, String propertyName, AggrFunction function) {
    this.aggregations
        .getSimpleDimensions()
        .add(
            new SimpleNamedExpression(
                retName, new AggrExpression(function, new PropertyReference(propertyName))));
  }

  public void groupBy(String propertyName, SearchRequest subRequest) {
    this.aggregations
        .getComplexDimensions()
        .add(new SimpleNamedExpression(propertyName, new PropertyReference(propertyName)));
    this.propagateDimensions.put(propertyName, subRequest);
  }

  public void addAggregate(String retName, String propertyName, AggrFunction function) {
    addAggregate(
        new SimpleNamedExpression(
            retName, new AggrExpression(function, new PropertyReference(propertyName))));
  }

  public BaseRequest<T> count() {
    countProperty("count", BaseEntity.ID_PROPERTY);
    return this;
  }

  public BaseRequest<T> count(String retName) {
    countProperty(retName, BaseEntity.ID_PROPERTY);
    return this;
  }

  public void countProperty(String propertyName) {
    countProperty(propertyName, propertyName);
  }

  public void countProperty(String retName, String propertyName) {
    addAggregate(retName, propertyName, AggrFunction.COUNT);
  }

  public void sum(String propertyName) {
    sum(propertyName, propertyName);
  }

  public void sum(String retName, String propertyName) {
    addAggregate(retName, propertyName, AggrFunction.SUM);
  }

  public void min(String propertyName) {
    min(propertyName, propertyName);
  }

  public void min(String retName, String propertyName) {
    addAggregate(retName, propertyName, AggrFunction.MIN);
  }

  public void max(String propertyName) {
    max(propertyName, propertyName);
  }

  public void max(String retName, String propertyName) {
    addAggregate(retName, propertyName, AggrFunction.MAX);
  }

  public void avg(String propertyName) {
    avg(propertyName, propertyName);
  }

  public void avg(String retName, String propertyName) {
    addAggregate(retName, propertyName, AggrFunction.AVG);
  }


  public void standardDeviation(String retName, String propertyName) {
    addAggregate(retName, propertyName, AggrFunction.STDDEV);
  }
  public void squareRootOfPopulationStandardDeviation(String retName, String propertyName) {
    addAggregate(retName, propertyName, AggrFunction.STDDEV_POP);
  }
  public void sampleVariance(String retName, String propertyName) {
    addAggregate(retName, propertyName, AggrFunction.VAR_SAMP);
  }
  public void samplePopulationVariance(String retName, String propertyName) {
    addAggregate(retName, propertyName, AggrFunction.VAR_POP);
  }
  public void standardDeviation(String propertyName) {
    standardDeviation(propertyName, propertyName);
  }

  public void squareRootOfPopulationStandardDeviation(String propertyName) {
    squareRootOfPopulationStandardDeviation(propertyName, propertyName);
  }

  public void sampleVariance(String propertyName) {
    sampleVariance(propertyName, propertyName);
  }

  public void samplePopulationVariance(String propertyName) {
    samplePopulationVariance(propertyName, propertyName);
  }
  


  protected BaseRequest<T> matchType(String... types) {
    appendSearchCriteria(new TypeCriteria(new Parameter("subTypes", types, Operator.IN)));
    return this;
  }

  protected BaseRequest<T> withTypeGroup() {
    this.aggregations
        .getSimpleDimensions()
        .add(new SimpleNamedExpression("_type", new PropertyReference("_child_type")));
    return this;
  }

  protected Optional<PropertyDescriptor> getProperty(String property) {
    EntityDescriptor entityDescriptor = getEntityDescriptor();
    while (entityDescriptor != null) {
      PropertyDescriptor propertyDescriptor = entityDescriptor.findProperty(property);
      if (propertyDescriptor != null) {
        return Optional.of(propertyDescriptor);
      }
      entityDescriptor = entityDescriptor.getParent();
    }
    return Optional.empty();
  }

  private EntityDescriptor getEntityDescriptor() {
    TQLResolver globalResolver = GLobalResolver.getGlobalResolver();
    if (globalResolver == null) {
      throw new TQLException("No global resolver registered");
    }
    return globalResolver.resolveEntityDescriptor(getTypeName());
  }

  public boolean isOneOfSelfField(String propertyName) {
    return getProperty(propertyName).isPresent();
  }

  public void setOffset(int offset) {
    if (slice == null) {
      slice = new Slice();
    }
    slice.setOffset(offset);
  }

  public void setSize(int size) {
    if (slice == null) {
      slice = new Slice();
    }
    slice.setSize(size);
  }

  public int getSize() {
    if (slice == null) {
      slice = new Slice();
    }
    return slice.getSize();
  }

  protected void addOrderBy(String property, boolean asc) {
    if (asc) {
      addOrderByAscending(property);
    } else {
      addOrderByDescending(property);
    }
  }

  protected boolean isDateTimeField(String fieldName) {
    PropertyDescriptor propertyDescriptor = getProperty(fieldName).get();
    return "true".equals(propertyDescriptor.getAdditionalInfo().get("isDate"));
  }

  public BaseRequest<T> unlimited() {
    this.slice = null;
    return this;
  }

  protected Optional<BaseRequest> subRequestOfFieldName(String fieldName) {
    Optional<PropertyDescriptor> propertyDescriptorOp = getProperty(fieldName);
    if (propertyDescriptorOp.isEmpty()) {
      throw new IllegalArgumentException(
          String.format(
              "The field '%s' of request type '%s' do not exists", fieldName, this.getTypeName()));
    }
    PropertyDescriptor propertyDescriptor = propertyDescriptorOp.get();
    Class returnType = propertyDescriptor.getType().javaType();
    TempRequest tempRequest =
        new TempRequest(returnType, ((Entity) ReflectUtil.newInstance(returnType)).typeName());
    tempRequest.selectProperty(BaseEntity.ID_PROPERTY);
    tempRequest.selectProperty(BaseEntity.VERSION_PROPERTY);
    tempRequest.appendSearchCriteria(
        createBasicSearchCriteria(BaseEntity.VERSION_PROPERTY, Operator.GREATER_THAN, 0l));
    tempRequest.unlimited();
    Relation relation = (Relation) propertyDescriptor;
    if (relation.getRelationKeeper() == getEntityDescriptor()) {
      this.appendSearchCriteria(
          new SubQuerySearchCriteria(fieldName, tempRequest, BaseEntity.ID_PROPERTY));
      return Optional.of(tempRequest);
    }

    this.appendSearchCriteria(
        new SubQuerySearchCriteria(
            BaseEntity.ID_PROPERTY, tempRequest, relation.getReverseProperty().getName()));

    return Optional.of(tempRequest);
  }

  public void setSlice(Slice slice) {
    this.slice = slice;
  }

  @Override
  public Map<String, SearchRequest> enhanceChildren() {
    return enhanceChildren;
  }

  public void enhanceSelf(BaseRequest<T> childRequest) {
    if (childRequest.getTypeName().equals(this.getTypeName())) {
      return;
    }
    enhanceChildren.put(childRequest.getTypeName(), childRequest);
  }
}
