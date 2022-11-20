package com.doublechaintech.data;

import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.ObjectUtil;
import com.doublechaintech.data.criteria.*;
import com.doublechaintech.data.sql.AggrFunction;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class BaseRequest<T extends Entity> implements SearchRequest<T> {

  // select properties
  List<SimpleNamedExpression> projections = new ArrayList<>();

  // simple dynamic properties
  List<SimpleNamedExpression> simpleDynamicProperties = new ArrayList<>();

  // search conditions
  SearchCriteria searchCriteria;

  // order by
  OrderBys orderBys = new OrderBys();

  // paging
  Page page;

  // enhance relations
  Map<String, SearchRequest> enhanceRelations = new HashMap<>();

  // enhance lists and partition by parent
  String partitionProperty;

  // basic return type
  Class<T> returnType;

  // aggregations
   Aggregations aggregations = new Aggregations();
   Map<String, SearchRequest> propagateAggregations = new HashMap<>();

  // group by, with aggregations
   Map<String, SearchRequest> propagateDimensions = new HashMap<>();

  public BaseRequest(Class<T> pReturnType) {
    returnType = pReturnType;
  }

  public void setReturnType(Class<T> pReturnType) {
    returnType = pReturnType;
  }

  @Override
  public Class<T> returnType() {
    return returnType;
  }

  // 尝试load 对象本身（存储自身的所有的表）
  public BaseRequest<T> selectSelf() {
    return this;
  }

  public void selectProperty(String propertyName) {
    if (ObjectUtil.isEmpty(propertyName)) {
      return;
    }
    if (this.projections.contains(propertyName)) {
      return;
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
  public Page getPage() {
    return page;
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

  public BaseRequest<T> top(int topN) {
    this.page = new Page();
    this.page.setSize(topN);
    return this;
  }

  public BaseRequest<T> page(int pageNumber, int pageSize) {
    this.page = new Page();
    this.page.setNumber(pageNumber);
    this.page.setSize(pageSize);
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

  @Override
  public List<SimpleNamedExpression> getSimpleDynamicProperties() {
    return simpleDynamicProperties;
  }

  public void addSimpleDynamicProperty(String name, Expression expression) {
    this.simpleDynamicProperties.add(new SimpleNamedExpression(name, expression));
  }

  public SearchCriteria createBasicSearchCriteria(String property, Operator operator, Object... values) {
    if (operator.hasOneOperator()){
      return new OneOperatorCriteria(operator, new PropertyReference(property));
    }else if (operator.hasTwoOperator()){
      return new TwoOperatorCriteria(operator, new PropertyReference(property), new Parameter(property, values, operator.hasMultiValue()));
    }else if (operator.isBetween()){
      if (ArrayUtil.length(values) != 2){
        throw new RepositoryException("Between需要下限和上限两个参数");
      }
      return new Between(new PropertyReference(property), new Parameter(property, values[0]), new Parameter(property, values[1]));
    }
    throw new RepositoryException("不支持的operator:" + operator);
  }

  public void addAggregate(SimpleNamedExpression aggregate){
    getAggregations().getAggregates().add(aggregate);
  }

  public void addAggregate(String retName, String propertyName, AggrFunction function){
    addAggregate(new SimpleNamedExpression(retName, new AggrExpression(function, new PropertyReference(propertyName))));
  }

  public BaseRequest<T> count(){
    countProperty("count", BaseEntity.ID_PROPERTY);
    return this;
  }


  public BaseRequest<T> count(String retName){
    countProperty(retName, BaseEntity.ID_PROPERTY);
    return this;
  }

  public void countProperty(String propertyName){
    countProperty(propertyName, propertyName);
  }

  public void countProperty(String retName, String propertyName){
    addAggregate(retName, propertyName, AggrFunction.COUNT);
  }

  public void sum(String propertyName){
    sum(propertyName, propertyName);
  }

  public void sum(String retName, String propertyName){
    addAggregate(retName, propertyName, AggrFunction.SUM);
  }
}
