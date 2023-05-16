package io.teaql.data;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.util.IdUtil;
import io.teaql.data.meta.EntityDescriptor;
import java.util.Collection;

public interface Repository<T extends Entity> {

  EntityDescriptor getEntityDescriptor();

  default Long prepareId(UserContext userContext, T entity) {
    if (entity.getId() != null) {
      return entity.getId();
    }

    Long id = userContext.generateId(entity);
    if (id != null) {
      return id;
    }

    return IdUtil.getSnowflakeNextId();
  }

  default Entity save(UserContext userContext, T entity) {
    if (entity == null) {
      return null;
    }
    save(userContext, ListUtil.of(entity));
    return entity;
  }

  Collection<T> save(UserContext userContext, Collection<T> entities);

  default void delete(UserContext userContext, T entity) {
    delete(userContext, ListUtil.of(entity));
  }

  void delete(UserContext userContext, Collection<T> entities);

  default void recover(UserContext userContext, T entity) {
    recover(userContext, ListUtil.of(entity));
  }

  void recover(UserContext userContext, Collection<T> entities);

  T execute(UserContext userContext, SearchRequest<T> request);

  SmartList<T> executeForList(UserContext userContext, SearchRequest<T> request);

  AggregationResult aggregation(UserContext userContext, SearchRequest<T> request);
}
