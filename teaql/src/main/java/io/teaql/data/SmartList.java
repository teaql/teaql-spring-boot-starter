package io.teaql.data;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.collection.CollectionUtil;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

public class SmartList<T extends Entity> implements Iterable<T> {
  List<T> data = new ArrayList<T>();

  List<AggregationResult> aggregationResults = new ArrayList<>();

  public SmartList() {}

  public SmartList(List<T> data) {
    if (data != null) {
      this.data.addAll(data);
    }
  }

  @Override
  public Iterator<T> iterator() {
    return data.iterator();
  }

  public T first() {
    return CollectionUtil.getFirst(data);
  }

  public boolean isEmpty() {
    return data.isEmpty();
  }

  public Stream<T> stream() {
    return data.stream();
  }

  public <R> Map<R, T> identityMap(Function<T, R> key) {
    return CollStreamUtil.toIdentityMap(data, key);
  }

  public Map<Long, T> mapById() {
    return identityMap(Entity::getId);
  }

  public <R> Map<R, List<T>> groupBy(Function<T, R> key) {
    return CollStreamUtil.groupByKey(data, key, false);
  }

  public void add(T pValue) {
    data.add(pValue);
  }

  public List<T> getData() {
    return data;
  }

  public void setData(List<T> pData) {
    data = pData;
  }

  public void addAggregationResult(UserContext userContext, AggregationResult aggregationResult) {
    aggregationResults.add(aggregationResult);
  }

  public List<AggregationResult> getAggregationResults() {
    return aggregationResults;
  }

  public void setAggregationResults(List<AggregationResult> pAggregationResults) {
    aggregationResults = pAggregationResults;
  }
}
