package io.teaql.memory;

import io.teaql.data.*;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.repository.AbstractRepository;
import java.util.Collection;

public class MemoryRepository<T extends BaseEntity> extends AbstractRepository<T> {

  private EntityDescriptor entityDescriptor;

  public MemoryRepository(EntityDescriptor pEntityDescriptor) {
    entityDescriptor = pEntityDescriptor;
  }

  @Override
  public EntityDescriptor getEntityDescriptor() {
    return entityDescriptor;
  }

  @Override
  protected void updateInternal(UserContext ctx, Collection<T> items) {}

  @Override
  protected void createInternal(UserContext ctx, Collection<T> items) {}

  @Override
  protected void deleteInternal(UserContext userContext, Collection<T> deleteItems) {}

  @Override
  protected void recoverInternal(UserContext userContext, Collection<T> recoverItems) {}

  @Override
  protected SmartList<T> loadInternal(UserContext userContext, SearchRequest<T> request) {
    return null;
  }

  @Override
  protected AggregationResult aggregateInternal(UserContext userContext, SearchRequest<T> request) {
    return null;
  }
}
