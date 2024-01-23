package io.teaql.data.memory.filter;

import io.teaql.data.Entity;
import io.teaql.data.SearchCriteria;

public interface Filter<T extends Entity> {
  boolean accept(T entity, SearchCriteria searchCriteria);
}
