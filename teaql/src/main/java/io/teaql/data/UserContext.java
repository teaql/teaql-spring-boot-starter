package io.teaql.data;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.getter.OptNullBasicTypeFromObjectGetter;
import cn.hutool.core.lang.caller.CallerUtil;
import cn.hutool.core.util.*;
import cn.hutool.log.LogFactory;
import cn.hutool.log.StaticLog;
import io.teaql.data.checker.CheckException;
import io.teaql.data.checker.CheckResult;
import io.teaql.data.checker.Checker;
import io.teaql.data.checker.ObjectLocation;
import io.teaql.data.criteria.Operator;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.EntityMetaFactory;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.translation.TranslationRequest;
import io.teaql.data.translation.TranslationResponse;
import io.teaql.data.translation.Translator;
import io.teaql.data.web.DuplicatedFormException;
import io.teaql.data.web.ErrorMessageException;
import io.teaql.data.web.UserContextInitializer;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

public class UserContext
    implements NaturalLanguageTranslator,
        RequestHolder,
        OptNullBasicTypeFromObjectGetter<String>,
        Translator {
  public static final String X_CLASS = "X-Class";
  private TQLResolver resolver = GLobalResolver.getGlobalResolver();
  private Map<String, Object> localStorage = new ConcurrentHashMap<>();

  public static final String REQUEST_HOLDER = "$request:requestHolder";

  public static final String RESPONSE_HOLDER = "$response:responseHolder";

  public Repository resolveRepository(String type) {
    if (resolver != null) {
      Repository repository = resolver.resolveRepository(type);
      if (repository != null) {
        return repository;
      }
    }
    throw new RepositoryException("Repository for '" + type + "' is not defined.");
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
    return RepositoryAdaptor.execute(this, edit(searchRequest));
  }

  public <T extends Entity> SearchRequest<T> edit(SearchRequest<T> request) {
    return request;
  }

  public <T extends Entity> SmartList<T> executeForList(SearchRequest searchRequest) {
    return RepositoryAdaptor.executeForList(this, edit(searchRequest));
  }

  public <T extends Entity> Stream<T> executeForStream(SearchRequest searchRequest) {
    return RepositoryAdaptor.executeForStream(this, edit(searchRequest));
  }

  public <T extends Entity> Stream<T> executeForStream(
      SearchRequest searchRequest, int enhanceBatch) {
    return RepositoryAdaptor.executeForStream(this, edit(searchRequest), enhanceBatch);
  }

  public void delete(Entity pEntity) {
    RepositoryAdaptor.delete(this, pEntity);
  }

  public void info(String messageTemplate, Object... args) {
    Class<?> caller = CallerUtil.getCaller(2);
    StaticLog.info(LogFactory.get(caller), messageTemplate, args);
  }

  public void debug(String messageTemplate, Object... args) {
    Class<?> caller = CallerUtil.getCaller(2);
    StaticLog.debug(LogFactory.get(caller), messageTemplate, args);
  }

  public void warn(String messageTemplate, Object... args) {
    Class<?> caller = CallerUtil.getCaller(2);
    StaticLog.warn(LogFactory.get(caller), messageTemplate, args);
  }

  public void warn(Exception e, String messageTemplate, Object... args) {
    Class<?> caller = CallerUtil.getCaller(2);
    StaticLog.warn(LogFactory.get(caller), e, messageTemplate, args);
  }

  public void error(String messageTemplate, Object... args) {
    Class<?> caller = CallerUtil.getCaller(2);
    StaticLog.error(LogFactory.get(caller), messageTemplate, args);
  }

  public void error(Exception e, String messageTemplate, Object... args) {
    Class<?> caller = CallerUtil.getCaller(2);
    StaticLog.error(LogFactory.get(caller), e, messageTemplate, args);
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

  public <T> T getBean(String name) {
    if (resolver != null) {
      T bean = resolver.getBean(name);
      if (bean != null) {
        return bean;
      }
    }
    throw new TQLException("No bean defined for name:" + name);
  }

  public LocalDateTime now() {
    return LocalDateTime.now();
  }

  public void checkAndFix(Entity entity) {
    if (!(entity instanceof BaseEntity)) {
      return;
    }
    Checker checker = getChecker(entity);
    if (ObjectUtil.isEmpty(checker)) {
      throw new TQLException("No checker defined for entity:" + entity);
    }
    checker.checkAndFix(this, (BaseEntity) entity);
    List errors = getList(Checker.TEAQL_DATA_CHECK_RESULT);
    if (ObjectUtil.isEmpty(errors)) {
      return;
    }
    localStorage.remove(Checker.TEAQL_DATA_CHECK_RESULT);
    errors = translateError(entity, errors);
    throw new CheckException(errors);
  }

  public void checkAndFix(Iterable<? extends Entity> entities) {
    if (ObjectUtil.isEmpty(entities)) {
      return;
    }

    int i = 0;
    for (Entity entity : entities) {
      if (!(entity instanceof BaseEntity)) {
        i++;
        continue;
      }
      Checker checker = getChecker(entity);
      if (ObjectUtil.isEmpty(checker)) {
        throw new TQLException("No checker defined for entity:" + entity);
      }
      checker.checkAndFix(this, (BaseEntity) entity, ObjectLocation.arrayRoot(i));
      i++;
    }

    List errors = getList(Checker.TEAQL_DATA_CHECK_RESULT);
    if (ObjectUtil.isEmpty(errors)) {
      return;
    }
    localStorage.remove(Checker.TEAQL_DATA_CHECK_RESULT);
    errors = translateError(null, errors);
    throw new CheckException(errors);
  }

  public Checker getChecker(Entity entity) {
    String name = entity.getClass().getName();
    Checker checker = getBean(ClassUtil.loadClass(name + "Checker"));
    return checker;
  }

  public List<CheckResult> translateError(Entity pEntity, List<CheckResult> errors) {
    return getNaturalLanguageTranslator(pEntity).translateError(pEntity, errors);
  }

  public NaturalLanguageTranslator getNaturalLanguageTranslator(Entity entity) {
    if (entity != null) {
      EntityDescriptor entityDescriptor = resolveEntityDescriptor(entity.typeName());
      if (BooleanUtil.toBoolean(entityDescriptor.getStr("viewObject", "false"))) {
        return new SimpleChineseViewTranslator(getBean(EntityMetaFactory.class));
      }
    }
    return new EnglishTranslator();
  }

  public void init(Object request) {
    List<UserContextInitializer> initializers =
        getResolver().getBeans(UserContextInitializer.class);
    if (initializers != null) {
      for (UserContextInitializer initializer : initializers) {
        if (initializer.support(request)) {
          initializer.init(this, request);
        }
      }
    }
  }

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

  public RequestHolder getRequestHolder() {
    RequestHolder requestHolder = (RequestHolder) getObj(REQUEST_HOLDER);
    if (requestHolder == null) {
      throw new IllegalStateException("user context missing request holder");
    }
    return requestHolder;
  }

  public ResponseHolder getResponseHolder() {
    ResponseHolder responseHolder = (ResponseHolder) getObj(RESPONSE_HOLDER);
    if (responseHolder == null) {
      throw new IllegalStateException("user context missing response holder");
    }
    return responseHolder;
  }

  @Override
  public String getHeader(String name) {
    return getRequestHolder().getHeader(name);
  }

  @Override
  public byte[] getPart(String name) {
    return getRequestHolder().getPart(name);
  }

  @Override
  public String getParameter(String name) {
    return getRequestHolder().getParameter(name);
  }

  @Override
  public byte[] getBodyBytes() {
    return getRequestHolder().getBodyBytes();
  }

  public void setResponseHeader(String headerName, String headerValue) {
    getResponseHolder().setHeader(headerName, headerValue);
  }

  public Object graphql(String query) {
    GraphQLService service = getBean(GraphQLService.class);
    if (service == null) {
      throw new TQLException("graphql service not found");
    }
    return service.execute(this, query);
  }

  public Object getObj(String key, Object defaultValue) {
    Object o = localStorage.get(key);
    if (o != null) {
      return o;
    }
    return defaultValue;
  }

  @Override
  public TranslationResponse translate(TranslationRequest req) {
    Translator translator = getBean(Translator.class);
    if (translator != null) {
      return translator.translate(req);
    }
    return null;
  }

  public void beforeCreate(EntityDescriptor descriptor, Entity toBeCreate) {}

  public void beforeUpdate(EntityDescriptor descriptor, Entity toBeUpdated) {}

  public void beforeDelete(EntityDescriptor descriptor, Entity toBeDeleted) {}

  public void beforeRecover(EntityDescriptor descriptor, Entity pToBeRecoverItem) {}

  public void afterLoad(EntityDescriptor descriptor, Entity loadedItem) {}

  public <T> T getInStore(String key) {
    return getBean(DataStore.class).get(key);
  }

  public <T> T getAndRemoveInStore(String key) {
    return getBean(DataStore.class).getAndRemove(key);
  }

  public void clearInStore(String key) {
    getBean(DataStore.class).remove(key);
  }

  public void putInStore(String key, Object value, int timeout) {
    getBean(DataStore.class).put(key, value, timeout);
  }

  public void duplicateFormException() {
    throw new DuplicatedFormException(
        "Your form is submitted and processing, please don't resubmit.");
  }

  public void error(String message) {
    throw new ErrorMessageException(message);
  }

  /**
   * reload the entity if id exists
   *
   * @param entity
   * @return
   * @param <T>
   */
  public <T extends BaseEntity> T reload(T entity) {
    if (entity == null) {
      return null;
    }
    Long id = entity.getId();
    if (id == null) {
      return entity;
    }

    if (entity.get$status().equals(EntityStatus.PERSISTED)
        || entity.get$status().equals(EntityStatus.PERSISTED_DELETED)) {
      return entity;
    }

    BaseRequest<T> tempRequest = initRequest(entity.getClass());
    tempRequest.appendSearchCriteria(
        tempRequest.createBasicSearchCriteria(BaseEntity.ID_PROPERTY, Operator.EQUAL, id));
    T item = tempRequest.execute(this);
    EntityDescriptor entityDescriptor = resolveEntityDescriptor(entity.typeName());
    while (entityDescriptor != null) {
      List<PropertyDescriptor> properties = entityDescriptor.getProperties();
      for (PropertyDescriptor property : properties) {
        entity.setProperty(property.getName(), item.getProperty(property.getName()));
      }
      entityDescriptor = entityDescriptor.getParent();
    }
    entity.set$status(item.get$status());
    return entity;
  }

  public <T> BaseRequest initRequest(Class<T> type) {
    if (type == null) {
      return null;
    }
    String name = type.getName();
    BaseRequest request = ReflectUtil.newInstance(ClassUtil.loadClass(name + "Request"), type);
    request.selectSelf();
    return request;
  }
}
