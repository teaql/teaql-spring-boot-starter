package io.teaql.data.value;

import io.teaql.data.BaseEntity;
import io.teaql.data.UserContext;

public interface BaseEntityExpression<T, U extends BaseEntity> extends Expression<T, U> {
    default Expression<T, Long> getId() {
        return apply(BaseEntity::getId);
    }

    default Expression<T, Long> getVersion() {
        return apply(BaseEntity::getVersion);
    }

    default Expression<T, U> save(UserContext userContext) {
        return apply(entity -> (U) entity.save(userContext));
    }

    default Expression<T, U> updateId(Long id) {
        return apply(
                entity -> {
                    entity.setId(id);
                    return entity;
                });
    }
}
