package io.teaql.data.internal;

import io.teaql.data.BaseRequest;
import io.teaql.data.OrderBys;
import io.teaql.data.SearchCriteria;
import io.teaql.data.SearchRequest;

public class TempRequest extends BaseRequest {
    String type;

    public TempRequest(SearchRequest request) {
        super(request.returnType());
        type = request.getTypeName();
        copy(request);
    }

    public TempRequest(Class returnType, String typeName) {
        super(returnType);
        type = typeName;
    }

    private void copy(SearchRequest pRequest) {
        projections.addAll(pRequest.getProjections());
        simpleDynamicProperties.addAll(pRequest.getSimpleDynamicProperties());
        searchCriteria = pRequest.getSearchCriteria();
        orderBys = pRequest.getOrderBy();
        slice = pRequest.getSlice();
        enhanceRelations = pRequest.enhanceRelations();
        partitionProperty = pRequest.getPartitionProperty();
        aggregations = pRequest.getAggregations();
        propagateAggregations = pRequest.getPropagateAggregations();
        propagateDimensions = pRequest.getPropagateDimensions();
        dynamicAggregateAttributes = pRequest.getDynamicAggregateAttributes();
        enhanceChildren = pRequest.enhanceChildren();
        cacheAggregation = pRequest.tryCacheAggregation();
        aggregateCacheTime = pRequest.getAggregateCacheTime();
        rawSql = pRequest.getRawSql();
        facetRequests = pRequest.getFacetRequests();
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
        }
        else {
            this.searchCriteria = SearchCriteria.and(this.searchCriteria, searchCriteria);
        }
        return this;
    }

    public void setOrderBy(OrderBys orderBy) {
        orderBys = orderBy;
    }
}
