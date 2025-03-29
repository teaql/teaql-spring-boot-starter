package io.teaql.data;

import java.util.Collection;
import java.util.stream.Stream;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.util.IdUtil;
import cn.hutool.core.util.ObjectUtil;

import io.teaql.data.meta.EntityDescriptor;

public interface Repository<T extends Entity> {

    EntityDescriptor getEntityDescriptor();

    Collection<T> save(UserContext userContext, Collection<T> entities);

    SmartList<T> executeForList(UserContext userContext, SearchRequest<T> request);

    default Stream<T> executeForStream(UserContext userContext, SearchRequest<T> request) {
        return executeForStream(userContext, request, 1000);
    }

    Stream<T> executeForStream(
            UserContext userContext, SearchRequest<T> request, int enhanceBatchSize);

    AggregationResult aggregation(UserContext userContext, SearchRequest<T> request);

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

    default void delete(UserContext userContext, T entity) {
        if (ObjectUtil.isEmpty(entity)) {
            return;
        }
        delete(userContext, ListUtil.of(entity));
    }

    default void delete(UserContext userContext, Collection<T> entities) {
        if (ObjectUtil.isNotEmpty(entities)) {
            for (T entity : entities) {
                entity.markAsDeleted();
            }
        }
        save(userContext, entities);
    }

    default void recover(UserContext userContext, T entity) {
        if (ObjectUtil.isEmpty(entity)) {
            return;
        }
        recover(userContext, ListUtil.of(entity));
    }

    default void recover(UserContext userContext, Collection<T> entities) {
        if (ObjectUtil.isNotEmpty(entities)) {
            for (T entity : entities) {
                entity.markAsRecover();
            }
        }
        save(userContext, entities);
    }

    default T execute(UserContext userContext, SearchRequest<T> request) {
        if (request instanceof BaseRequest) {
            ((BaseRequest<T>) request).top(1);
        }
        return executeForList(userContext, request).first();
    }
}
