package io.teaql.data;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.map.MapUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.ObjectUtil;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.Relation;
import java.util.*;
import java.util.stream.Stream;

public class RepositoryAdaptor {

  public static <T extends Entity> void saveGraph(UserContext userContext, Object items) {
    if (ObjectUtil.isEmpty(items)) {
      return;
    }
    Map<String, List<Entity>> entities = new HashMap<>();

    // collect the items to persist
    collect(userContext, entities, items, new ArrayList<>());
    for (Map.Entry<String, List<Entity>> entry : entities.entrySet()) {
      String type = entry.getKey();
      List<Entity> list = entry.getValue();
      Repository<Entity> repository = userContext.resolveRepository(type);
      for (Entity entity : list) {
        Long id = repository.prepareId(userContext, entity);
        entity.setId(id);
      }
    }

    for (Map.Entry<String, List<Entity>> entry : entities.entrySet()) {
      String type = entry.getKey();
      List<Entity> list = entry.getValue();
      Repository<Entity> repository = userContext.resolveRepository(type);
      Collection<Entity> saveResult = repository.save(userContext, list);
      Map<Long, Entity> entityMap = CollStreamUtil.toIdentityMap(list, Entity::getId);
      for (Entity entity : saveResult) {
        Entity input = entityMap.get(entity.getId());
        if (input == entity) {
          continue;
        }
        copyProperties(entity, input);
      }
    }
  }

  private static void copyProperties(Entity src, Entity dest) {}

  private static void collect(
      UserContext userContext, Map<String, List<Entity>> entities, Object item, List handled) {
    if (item == null) {
      return;
    }
    for (Object o : handled) {
      if (item == o) {
        return;
      }
    }
    handled.add(item);
    if (item instanceof Entity) {
      Entity entity = (Entity) item;
      appendEntity(userContext, entities, entity, handled);
    } else if (item instanceof Iterable) {
      for (Object entity : (Iterable) item) {
        collect(userContext, entities, entity, handled);
      }
    } else if (ArrayUtil.isArray(item)) {
      int length = ArrayUtil.length(item);
      for (int i = 0; i < length; i++) {
        Object o = ArrayUtil.get(item, i);
        collect(userContext, entities, o, handled);
      }
    } else if (item instanceof Iterator) {
      while (((Iterator) item).hasNext()) {
        collect(userContext, entities, ((Iterator<?>) item).next(), handled);
      }
    } else if (item instanceof Map) {
      Map m = (Map) item;
      m.forEach(
          (k, v) -> {
            collect(userContext, entities, k, handled);
            collect(userContext, entities, v, handled);
          });
    }
  }

  private static void appendEntity(
      UserContext userContext, Map<String, List<Entity>> entities, Entity entity, List pHandled) {
    if (entity == null) {
      return;
    }

    String typeName = entity.typeName();
    List<Entity> list = entities.get(typeName);
    if (list == null) {
      list = new ArrayList<>();
      entities.put(typeName, list);
    }
    if (entity.needPersist()) {
      list.add(entity);
    }

    EntityDescriptor entityDescriptor = userContext.resolveEntityDescriptor(typeName);
    while (entityDescriptor != null) {
      List<PropertyDescriptor> properties = entityDescriptor.getProperties();
      for (PropertyDescriptor property : properties) {
        if (property instanceof Relation r) {
          EntityDescriptor relationKeeper = r.getRelationKeeper();
          Boolean isView = MapUtil.getBool(relationKeeper.getAdditionalInfo(), "view");
          if (isView != null && isView) {
            continue;
          }
        }
        String name = property.getName();
        Object propertyValue = entity.getProperty(name);
        collect(userContext, entities, propertyValue, pHandled);
      }
      entityDescriptor = entityDescriptor.getParent();
    }
  }

  public static <T extends Entity> void delete(UserContext userContext, T entity) {
    Repository repository = userContext.resolveRepository(entity.typeName());
    repository.delete(userContext, entity);
  }

  public static <T extends Entity> T execute(UserContext userContext, SearchRequest<T> request) {
    Repository<T> repository = userContext.resolveRepository(request.getTypeName());
    return repository.execute(userContext, request);
  }

  public static <T extends Entity> SmartList<T> executeForList(
      UserContext userContext, SearchRequest<T> request) {
    Repository<T> repository = userContext.resolveRepository(request.getTypeName());
    return repository.executeForList(userContext, request);
  }

  public static <T extends Entity> AggregationResult aggregation(
      UserContext userContext, SearchRequest<T> request) {
    Repository<T> repository = userContext.resolveRepository(request.getTypeName());
    return repository.aggregation(userContext, request);
  }

  public static <T extends Entity> Stream<T> executeForStream(
      UserContext userContext, SearchRequest request) {
    Repository<T> repository = userContext.resolveRepository(request.getTypeName());
    return repository.executeForStream(userContext, request);
  }

  public static <T extends Entity> Stream<T> executeForStream(
      UserContext userContext, SearchRequest request, int enhanceBatch) {
    Repository<T> repository = userContext.resolveRepository(request.getTypeName());
    return repository.executeForStream(userContext, request, enhanceBatch);
  }
}
