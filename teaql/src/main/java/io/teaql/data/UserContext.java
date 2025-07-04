package io.teaql.data;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.spi.LocationAwareLogger;

import cn.hutool.cache.Cache;
import cn.hutool.cache.CacheUtil;
import cn.hutool.core.codec.Base64;
import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.getter.OptNullBasicTypeFromObjectGetter;
import cn.hutool.core.lang.caller.CallerUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.BooleanUtil;
import cn.hutool.core.util.ClassUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;

import io.teaql.data.checker.CheckException;
import io.teaql.data.checker.CheckResult;
import io.teaql.data.checker.Checker;
import io.teaql.data.checker.ObjectLocation;
import io.teaql.data.criteria.Operator;
import io.teaql.data.lock.LockService;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.EntityMetaFactory;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.translation.TranslationRequest;
import io.teaql.data.translation.TranslationResponse;
import io.teaql.data.translation.Translator;
import io.teaql.data.web.DuplicatedFormException;
import io.teaql.data.web.ErrorMessageException;
import io.teaql.data.web.UserContextInitializer;

public class UserContext
        implements NaturalLanguageTranslator,
        RequestHolder,
        OptNullBasicTypeFromObjectGetter<String>,
        Translator {

    public static final String FQCN = UserContext.class.getName();
    public static final String X_CLASS = "X-Class";
    public static final String TOAST = "toast";
    public static final String REQUEST_HOLDER = "$request:requestHolder";
    public static final String RESPONSE_HOLDER = "$response:responseHolder";
    InternalIdGenerator internalIdGenerator;
    private TQLResolver resolver = GLobalResolver.getGlobalResolver();
    private final Cache<String, Object> localStorage = CacheUtil.newTimedCache(0);

    public Repository resolveRepository(String type) {
        if (getResolver() != null) {
            Repository repository = getResolver().resolveRepository(type);
            if (repository != null) {
                return repository;
            }
        }
        throw new RepositoryException("Repository for '" + type + "' is not defined.");
    }

    public DataConfigProperties config() {
        if (getResolver() != null) {
            DataConfigProperties bean = getResolver().getBean(DataConfigProperties.class);
            if (bean != null) {
                return bean;
            }
        }
        return new DataConfigProperties();
    }

    public EntityDescriptor resolveEntityDescriptor(String type) {
        if (getResolver() != null) {
            EntityDescriptor entityDescriptor = getResolver().resolveEntityDescriptor(type);
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
        LocationAwareLogger logger = getLogger();
        logger.log(null, FQCN, LocationAwareLogger.INFO_INT, messageTemplate, args, null);
    }

    public void info(Marker marker, String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(marker, FQCN, LocationAwareLogger.INFO_INT, messageTemplate, args, null);
    }

    public void debug(String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(null, FQCN, LocationAwareLogger.DEBUG_INT, messageTemplate, args, null);
    }

    public void debug(Marker marker, String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(marker, FQCN, LocationAwareLogger.DEBUG_INT, messageTemplate, args, null);
    }

    public void warn(String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(null, FQCN, LocationAwareLogger.WARN_INT, messageTemplate, args, null);
    }

    public void warn(Marker marker, String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(marker, FQCN, LocationAwareLogger.WARN_INT, messageTemplate, args, null);
    }

    public void warn(Exception e, String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(null, FQCN, LocationAwareLogger.WARN_INT, messageTemplate, args, e);
    }

    public void warn(Marker marker, Exception e, String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(marker, FQCN, LocationAwareLogger.WARN_INT, messageTemplate, args, e);
    }

    public void error(String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(null, FQCN, LocationAwareLogger.ERROR_INT, messageTemplate, args, null);
    }

    public void error(Marker marker, String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(marker, FQCN, LocationAwareLogger.ERROR_INT, messageTemplate, args, null);
    }

    public void error(Exception e, String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(null, FQCN, LocationAwareLogger.ERROR_INT, messageTemplate, args, e);
    }

    public void error(Marker marker, Exception e, String messageTemplate, Object... args) {
        LocationAwareLogger logger = getLogger();
        logger.log(marker, FQCN, LocationAwareLogger.ERROR_INT, messageTemplate, args, e);
    }

    private LocationAwareLogger getLogger() {
        Class<?> caller = CallerUtil.getCaller(3);
        return (LocationAwareLogger) LoggerFactory.getLogger(caller);
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

    public void del(String key) {
        localStorage.remove(key);
    }

    public boolean containsKey(String key) {
        return localStorage.containsKey(key);
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
        }
        else if (!(existing instanceof Collection<?>)) {
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
        if (getResolver() != null) {
            T bean = getResolver().getBean(clazz);
            if (bean != null) {
                return bean;
            }
        }
        throw new TQLException("No bean defined for type:" + clazz);
    }

    public <T> T getBean(String name) {
        if (getResolver() != null) {
            T bean = getResolver().getBean(name);
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
        String type = entity.typeName();
        Map<String, Checker> checkers = getBean("checkers");
        if (checkers == null) {
            return null;
        }
        return checkers.get(type);
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

    public Long generateId(Entity pEntity) {
        if (this.internalIdGenerator == null) {
            return null;
        }
        return internalIdGenerator.generateId(pEntity);
    }

    public void sendEvent(Object event) {
    }

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

    public List<String> getHeaderNames() {
        return getRequestHolder().getHeaderNames();
    }

    @Override
    public String method() {
        return getRequestHolder().method();
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
    public List<String> getParameterNames() {
        return getRequestHolder().getParameterNames();
    }

    @Override
    public String getParameter(String name) {
        return getRequestHolder().getParameter(name);
    }

    @Override
    public byte[] getBodyBytes() {
        return getRequestHolder().getBodyBytes();
    }

    @Override
    public String requestUri() {
        return getRequestHolder().requestUri();
    }

    @Override
    public String getRemoteAddress() {
        return getRequestHolder().getRemoteAddress();
    }

    public String getClientIp() {
        String header = getHeader("X-Forwarded-For");
        if (header == null) {
            return getRemoteAddress();
        }
        String[] parts = header.split(",");
        return parts[0];
    }

    public boolean isFromLocalhost() {
        String clientIp = getClientIp();
        try {
            return InetAddress.getByName(clientIp).isLoopbackAddress();
        }
        catch (UnknownHostException pE) {
            throw new RuntimeException(pE);
        }
    }

    public List<String> getProxyChain() {
        String header = getHeader("X-Forwarded-For");
        if (header == null) {
            return Collections.emptyList();
        }
        String[] parts = header.split(",");
        return ListUtil.of(ArrayUtil.sub(parts, 1, -1));
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
        return get(key, () -> defaultValue);
    }

    public <T> T get(String key, Supplier<T> supplier) {
        return (T) localStorage.get(key, () -> supplier.get());
    }

    public <T> T advancedGet(String key, Supplier<T> supplier) {
        return get(key, () -> getInStore(key, supplier));
    }

    @Override
    public TranslationResponse translate(TranslationRequest req) {
        Translator translator = getBean(Translator.class);
        if (translator != null) {
            return translator.translate(req);
        }
        return null;
    }

    public void beforeCreate(EntityDescriptor descriptor, Entity toBeCreate) {
    }

    public void beforeUpdate(EntityDescriptor descriptor, Entity toBeUpdated) {
    }

    public void beforeDelete(EntityDescriptor descriptor, Entity toBeDeleted) {
    }

    public void beforeRecover(EntityDescriptor descriptor, Entity pToBeRecoverItem) {
    }

    public void afterLoad(EntityDescriptor descriptor, Entity loadedItem) {
    }

    public <T> T getInStore(String key) {
        return getBean(DataStore.class).get(key);
    }

    public <T> T getInStore(String key, Supplier<T> supplier) {
        return getBean(DataStore.class).get(key, supplier);
    }

    public <T> T getAndRemoveInStore(String key) {
        return getBean(DataStore.class).getAndRemove(key);
    }

    public void clearInStore(String key) {
        getBean(DataStore.class).remove(key);
    }

    public void putInStore(String key, Object value, int timeout) {
        if (timeout <= 0) {
            getBean(DataStore.class).put(key, value);
        }
        else {
            getBean(DataStore.class).put(key, value, timeout);
        }
    }

    public void duplicateFormException() {
        throw new DuplicatedFormException(
                "Your form is submitted and processing, please don't resubmit.");
    }

    public void errorMessage(String message, Object... args) {
        String req = getStr("_req");
        if (req != null) {
            clearInStore(req);
        }
        throw new ErrorMessageException(StrUtil.format(message, args));
    }

    /**
     * reload the entity if id exists
     *
     * @param entity
     * @param <T>
     * @return
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
        request.appendSearchCriteria(
                request.createBasicSearchCriteria(BaseEntity.VERSION_PROPERTY, Operator.GREATER_THAN, 0l));
        return request;
    }

    /**
     * execute task directly(in the same thread)
     *
     * @param lock the lock/task name
     * @param task the task to execute
     */
    public void execLocalTask(String lock, Runnable task) {
        if (ObjectUtil.isEmpty(lock)) {
            throw new TQLException("lock cannot be empty to execute local task");
        }

        LockService lockService = getBean(LockService.class);
        Lock localLock = lockService.getLocalLock(this, lock);
        runTask(task, localLock);
    }

    /**
     * execute task in one thread of the pool
     *
     * @param lock the lock/task name
     * @param task the task to execute
     */
    public void execLocalTaskAsync(String lock, Runnable task) {
        if (ObjectUtil.isEmpty(lock)) {
            throw new TQLException("lock cannot be empty to execute local task");
        }

        LockService lockService = getBean(LockService.class);
        Lock localLock = lockService.getLocalLock(this, lock);
        runTaskAsync(task, localLock);
    }

    /**
     * execute task directly(in the same thread), if this task is running, then do nothing
     *
     * @param lock the lock/task name
     * @param task the task to execute
     */
    public void execSingleLocalTask(String lock, Runnable task) {
        if (ObjectUtil.isEmpty(lock)) {
            throw new TQLException("lock cannot be empty to execute local task");
        }
        LockService lockService = getBean(LockService.class);
        Lock localLock = lockService.getLocalLock(this, lock);
        runSingleTask(task, localLock);
    }

    /**
     * execute task in one thread of the pool
     *
     * @param lock the lock/task name
     * @param task the task to execute
     */
    public void execSingleLocalTaskAsync(String lock, Runnable task) {
        if (ObjectUtil.isEmpty(lock)) {
            throw new TQLException("lock cannot be empty to execute local task");
        }

        LockService lockService = getBean(LockService.class);
        Lock localLock = lockService.getLocalLock(this, lock);
        runSingleTaskAsync(task, localLock);
    }

    /**
     * execute global task directly(in the same thread)
     *
     * @param lock the lock/task name
     * @param task the task to execute
     */
    public void execGlobalTask(String lock, Runnable task) {
        if (ObjectUtil.isEmpty(lock)) {
            throw new TQLException("lock cannot be empty to execute global task");
        }

        LockService lockService = getBean(LockService.class);
        Lock distributeLock = lockService.getDistributeLock(this, lock);
        runTask(task, distributeLock);
    }

    /**
     * execute global task in one thread of the pool
     *
     * @param lock the lock/task name
     * @param task the task to execute
     */
    public void execGlobalTaskAsync(String lock, Runnable task) {
        if (ObjectUtil.isEmpty(lock)) {
            throw new TQLException("lock cannot be empty to execute global task");
        }

        LockService lockService = getBean(LockService.class);
        Lock distributeLock = lockService.getDistributeLock(this, lock);
        runTaskAsync(task, distributeLock);
    }

    /**
     * execute global task directly(in the same thread), if this task is running, then do nothing
     *
     * @param lock the lock/task name
     * @param task the task to execute
     */
    public void execSingleGlobalTask(String lock, Runnable task) {
        if (ObjectUtil.isEmpty(lock)) {
            throw new TQLException("lock cannot be empty to execute local task");
        }
        LockService lockService = getBean(LockService.class);
        Lock distributeLock = lockService.getDistributeLock(this, lock);
        runSingleTask(task, distributeLock);
    }

    /**
     * execute task in one thread of the pool, if this task is running, then do nothing
     *
     * @param lock the lock/task name
     * @param task the task to execute
     */
    public void execSingleGlobalTaskAsync(String lock, Runnable task) {
        if (ObjectUtil.isEmpty(lock)) {
            throw new TQLException("lock cannot be empty to execute local task");
        }

        LockService lockService = getBean(LockService.class);
        Lock distributeLock = lockService.getDistributeLock(this, lock);
        runSingleTaskAsync(task, distributeLock);
    }

    private void runTask(Runnable task, Lock lock) {
        if (task == null) {
            return;
        }
        try {
            if (lock != null) {
                lock.lock();
            }
            task.run();
        }
        finally {
            if (lock != null) {
                lock.unlock();
            }
        }
    }

    private void runTaskAsync(Runnable task, Lock lock) {
        LockService.taskExecutor.execute(() -> runTask(task, lock));
    }

    private void runSingleTask(Runnable task, Lock lock) {
        if (task == null) {
            return;
        }
        boolean ready = false;
        try {
            if (lock != null) {
                ready = lock.tryLock();
            }
            else {
                ready = true;
            }
            if (ready) {
                task.run();
            }
        }
        finally {
            if (lock != null && ready) {
                lock.unlock();
            }
        }
    }

    private void runSingleTaskAsync(Runnable task, Lock lock) {
        LockService.taskExecutor.execute(() -> runSingleTask(task, lock));
    }

    public void makeToast(String content, int duration, String type) {
        Map<String, Object> toast = new HashMap<>();
        toast.put("text", content);
        toast.put("duration", duration * 1000);
        toast.put("icon", type);
        toast.put("position", "center");
        toast.put("playSound", "success");
        setResponseHeader(TOAST, Base64.encode(JSONUtil.toJsonStr(toast)));
    }

    public void makeToast(String content) {
        makeToast(content, 3, "info");
    }

    public Object getToast() {
        return getObj(TOAST);
    }

    public Object back() {
        setResponseHeader("command", "back");
        return new HashMap<>();
    }

    public Object home() {
        setResponseHeader("command", "home");
        return new HashMap<>();
    }
}
