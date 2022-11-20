package com.doublechaintech.data;

import cn.hutool.core.util.StrUtil;
import cn.hutool.extra.spring.SpringUtil;
import com.doublechaintech.data.meta.EntityDescriptor;
import com.doublechaintech.data.meta.EntityMetaFactory;

import java.util.Map;

public class UserContext {

  public Repository resolveRepository(String type) {
    Map<String, Repository> beansOfType = SpringUtil.getBeansOfType(Repository.class);
    for (Repository value : beansOfType.values()) {
      if (value.getEntityDescriptor().getType().equals(type)) {
        return value;
      }
    }
    throw new RepositoryException("Repository for:" + type + " not defined.");
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
}
