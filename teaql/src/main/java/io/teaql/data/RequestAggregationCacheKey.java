package io.teaql.data;

import java.util.Objects;

public class RequestAggregationCacheKey extends TempRequest {
    public RequestAggregationCacheKey(SearchRequest request) {
        super(request);
    }

    @Override
    public boolean equals(Object pO) {
        if (this == pO) return true;
        if (!(pO instanceof BaseRequest<?> that)) return false;
        return Objects.equals(getProjections(), that.getProjections())
                && Objects.equals(getSimpleDynamicProperties(), that.getSimpleDynamicProperties())
                && Objects.equals(getSearchCriteria(), that.getSearchCriteria())
                && Objects.equals(orderBys, that.orderBys)
                && Objects.equals(enhanceRelations, that.enhanceRelations)
                && Objects.equals(getDynamicAggregateAttributes(), that.getDynamicAggregateAttributes())
                && Objects.equals(getPartitionProperty(), that.getPartitionProperty())
                && Objects.equals(returnType(), that.returnType())
                && Objects.equals(getAggregations(), that.getAggregations())
                && Objects.equals(getPropagateAggregations(), that.getPropagateAggregations())
                && Objects.equals(getPropagateDimensions(), that.getPropagateDimensions())
                && Objects.equals(enhanceChildren, that.enhanceChildren);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                getProjections(),
                getSimpleDynamicProperties(),
                getSearchCriteria(),
                orderBys,
                enhanceRelations,
                getDynamicAggregateAttributes(),
                getPartitionProperty(),
                returnType,
                getAggregations(),
                getPropagateAggregations(),
                getPropagateDimensions(),
                enhanceChildren);
    }
}
