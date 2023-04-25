package io.teaql.data;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.util.ClassUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.extra.spring.SpringUtil;
import io.teaql.data.checker.CheckException;
import io.teaql.data.checker.Checker;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.EntityMetaFactory;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class UserContext {
  private Map<String, Object> localStorage = new ConcurrentHashMap<>();

  public Repository resolveRepository(String type) {
    Map<String, Repository> beansOfType = SpringUtil.getBeansOfType(Repository.class);
    for (Repository value : beansOfType.values()) {
      if (value.getEntityDescriptor().getType().equals(type)) {
        return value;
      }
    }
    throw new RepositoryException("Repository for:" + type + " not defined.");
  }

  public DataConfig config() {
    return SpringUtil.getBean(DataConfig.class);
  }

  public EntityDescriptor resolveEntityDescriptor(String type) {
    EntityMetaFactory bean = SpringUtil.getBean(EntityMetaFactory.class);
    return bean.resolveEntityDescriptor(type);
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
    return SpringUtil.getBean(clazz);
  }

  public LocalDateTime now() {
    return LocalDateTime.now();
  }

  public void checkAndFix(Entity entity) {
    if (entity instanceof BaseEntity) {
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

  public void init(Object request) {}
}
