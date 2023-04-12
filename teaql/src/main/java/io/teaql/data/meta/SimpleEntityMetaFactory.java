package io.teaql.data.meta;

import io.teaql.data.RepositoryException;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class SimpleEntityMetaFactory implements EntityMetaFactory {
  Map<String, EntityDescriptor> registeredEntities = new ConcurrentHashMap<>();

  @Override
  public EntityDescriptor resolveEntityDescriptor(String type) {
    EntityDescriptor entityDescriptor = registeredEntities.get(type);
    if (entityDescriptor == null) {
      throw new RepositoryException("entityDescriptor " + type + " cannot be resolved");
    }
    return entityDescriptor;
  }

  public void register(EntityDescriptor entityDescriptor) {
    if (entityDescriptor == null) {
      return;
    }
    registeredEntities.put(entityDescriptor.getType(), entityDescriptor);
  }
}
