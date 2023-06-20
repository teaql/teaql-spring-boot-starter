package io.teaql.data;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.util.*;
import io.teaql.data.checker.CheckException;
import io.teaql.data.checker.Checker;
import io.teaql.data.meta.EntityDescriptor;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class UserContext {
  private TQLResolver resolver = GLobalResolver.getGlobalResolver();
  private Map<String, Object> localStorage = new ConcurrentHashMap<>();

  public Repository resolveRepository(String type) {
    if (resolver != null) {
      Repository repository = resolver.resolveRepository(type);
      if (repository != null) {
        return repository;
      }
    }
    throw new RepositoryException("Repository for:" + type + " not defined.");
  }

  public DataConfigProperties config() {
    if (resolver != null) {
      DataConfigProperties bean = resolver.getBean(DataConfigProperties.class);
      if (bean != null) {
        return bean;
      }
    }
    return new DataConfigProperties();
  }

  public EntityDescriptor resolveEntityDescriptor(String type) {
    if (resolver != null) {
      EntityDescriptor entityDescriptor = resolver.resolveEntityDescriptor(type);
      if (entityDescriptor != null) {
        return entityDescriptor;
      }
    }
    throw new RepositoryException("ItemDescriptor for:" + type + " not defined.");
  }

  public void saveGraph(Object items) {
    RepositoryAdaptor.saveGraph(this, items);
  }

  public <T extends Entity> T execute(SearchRequest<T> searchRequest) {
    return RepositoryAdaptor.execute(this, searchRequest);
  }

  public <T extends Entity> SmartList<T> executeForList(SearchRequest searchRequest) {
    return RepositoryAdaptor.executeForList(this, searchRequest);
  }

  public void delete(Entity pEntity) {
    RepositoryAdaptor.delete(this, pEntity);
  }

  public void info(String messageTemplate, Object... args) {
    System.out.println(StrUtil.format(messageTemplate, args));
  }

  public <T extends Entity> AggregationResult aggregation(SearchRequest request) {
    return RepositoryAdaptor.aggregation(this, request);
  }

  public void put(String key, Object value) {
    if (ObjectUtil.isEmpty(key)) {
      throw new IllegalArgumentException("key cannot be null");
    }
    localStorage.put(key, value);
  }

  public void append(String key, Object value) {
    if (ObjectUtil.isEmpty(key)) {
      throw new IllegalArgumentException("key cannot be null");
    }
    if (ObjectUtil.isEmpty(value)) {
      return;
    }
    Object existing = localStorage.get(key);
    if (existing == null) {
      existing = new ArrayList();
    } else if (!(existing instanceof Collection<?>)) {
      ArrayList newCollection = new ArrayList();
      newCollection.add(existing);
      existing = newCollection;
    }
    ((Collection) existing).add(value);
    localStorage.put(key, existing);
  }

  public List getList(String key) {
    Object value = localStorage.get(key);
    if (value == null) {
      return ListUtil.empty();
    }
    if (value instanceof List) {
      return (List) value;
    }
    List ret = new ArrayList();
    ret.add(value);
    return ret;
  }

  public boolean hasObject(String key, Object o) {
    List list = getList(key);
    for (Object o1 : list) {
      if (o1 == o) {
        return true;
      }
    }
    return false;
  }

  public <T> T getBean(Class<T> clazz) {
    if (resolver != null) {
      T bean = resolver.getBean(clazz);
      if (bean != null) {
        return bean;
      }
    }
    throw new TQLException("No bean defined for type:" + clazz);
  }

  public LocalDateTime now() {
    return LocalDateTime.now();
  }

  public void checkAndFix(Entity entity) {
    if (!(entity instanceof BaseEntity)) {
      return;
    }
    String name = entity.getClass().getName();
    Checker checker = getBean(ClassUtil.loadClass(name + "Checker"));
    checker.checkAndFix(this, (BaseEntity) entity);
    List errors = getList(Checker.TEAQL_DATA_CHECK_RESULT);
    if (ObjectUtil.isEmpty(errors)) {
      return;
    }
    localStorage.remove(Checker.TEAQL_DATA_CHECK_RESULT);
    throw new CheckException(errors);
  }

  public Object getObj(String key) {
    return getObj(key, null);
  }

  public Object getObj(String key, Object defaultValue) {
    Object o = localStorage.get(key);
    if (o != null) {
      return o;
    }
    return defaultValue;
  }

  public String getStr(String key) {
    return getStr(key, null);
  }

  public String getStr(String key, String defaultValue) {
    Object obj = getObj(key);
    if (obj == null) {
      return defaultValue;
    }
    return ObjectUtil.toString(obj);
  }

  public Integer getInt(String key) {
    return getInt(key, null);
  }

  public Integer getInt(String key, Integer defaultValue) {
    Object obj = getObj(key);
    if (obj == null) {
      return defaultValue;
    }
    if (obj instanceof Number) {
      return ((Number) obj).intValue();
    }
    return NumberUtil.parseInt(ObjectUtil.toString(obj));
  }

  public Boolean getBool(String key) {
    return getBool(key, null);
  }

  public Boolean getBool(String key, Boolean defaultValue) {
    Object obj = getObj(key);
    if (obj == null) {
      return defaultValue;
    }
    if (obj instanceof Boolean) {
      return (Boolean) obj;
    }
    return BooleanUtil.toBooleanObject(ObjectUtil.toString(obj));
  }

  public void init(Object request) {}

  public InternalIdGenerator getInternalIdGenerator() {
    return internalIdGenerator;
  }

  public void setInternalIdGenerator(InternalIdGenerator internalIdGenerator) {
    this.internalIdGenerator = internalIdGenerator;
  }

  InternalIdGenerator internalIdGenerator;

  public Long generateId(Entity pEntity) {
    if (this.internalIdGenerator == null) {
      return null;
    }
    return internalIdGenerator.generateId(pEntity);
  }

  public void sendEvent(Object event) {}

  public void afterPersist(BaseEntity item) {
    item.clearUpdatedProperties();
  }

  public TQLResolver getResolver() {
    return resolver;
  }

  public void setResolver(TQLResolver pResolver) {
    resolver = pResolver;
  }
}
