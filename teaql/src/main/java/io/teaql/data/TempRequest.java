package io.teaql.data;

public class TempRequest extends BaseRequest {
  String type;

  public TempRequest(SearchRequest request) {
    super(request.returnType());
    type = request.getTypeName();
    copy(request);
  }

  private void copy(SearchRequest pRequest) {
    projections.addAll(pRequest.getProjections());
    simpleDynamicProperties.addAll(pRequest.getSimpleDynamicProperties());
    searchCriteria = pRequest.getSearchCriteria();
    orderBys = pRequest.getOrderBy();
    page = pRequest.getPage();
    enhanceRelations = pRequest.enhanceRelations();
    partitionProperty = pRequest.getPartitionProperty();
    aggregations = pRequest.getAggregations();
    propagateAggregations = pRequest.getPropagateAggregations();
    propagateDimensions = pRequest.getPropagateDimensions();
    dynamicAggregateAttributes = pRequest.getDynamicAggregateAttributes();
  }

  public TempRequest(Class returnType, String typeName) {
    super(returnType);
    type = typeName;
  }

  @Override
  public String getTypeName() {
    return type;
  }

  @Override
  public BaseRequest appendSearchCriteria(SearchCriteria searchCriteria) {
    if (searchCriteria == null) {
      return this;
    }
    if (this.searchCriteria == null) {
      this.searchCriteria = searchCriteria;
    } else {
      this.searchCriteria = SearchCriteria.and(this.searchCriteria, searchCriteria);
    }
    return this;
  }
}
