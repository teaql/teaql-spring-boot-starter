package io.teaql.data.quarkus;

import javax.sql.DataSource;

import jakarta.enterprise.context.ApplicationScoped;
import jakarta.enterprise.inject.Produces;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import io.teaql.data.DataConfigProperties;
import io.teaql.data.DataStore;
import io.teaql.data.PurposeRequestPolicy;
import io.teaql.data.RequestPolicy;
import io.teaql.data.TQLResolver;
import io.teaql.data.UserContext;
import io.teaql.data.TeaQLDatabase;
import io.teaql.data.lock.LockService;
import io.teaql.data.lock.SimpleLockService;
import io.teaql.data.log.LogManager;
import io.teaql.data.meta.SimpleEntityMetaFactory;
import io.teaql.data.meta.EntityMetaFactory;
import io.teaql.data.translation.Translator;
import io.teaql.data.utils.CacheUtil;
import io.teaql.data.utils.TimedCache;

/**
 * Quarkus CDI Producer. Provides default beans for the TeaQL runtime.
 * Application can @Override any bean.
 */
@ApplicationScoped
public class TeaQLProducer {

    @Inject
    DataSource dataSource;

    @Produces
    @Singleton
    public TeaQLDatabase teaQLDatabase() {
        return new QuarkusDatabase(dataSource);
    }

    @Produces
    @Singleton
    public EntityMetaFactory entityMetaFactory() {
        return new SimpleEntityMetaFactory();
    }

    @Produces
    @ApplicationScoped
    public DataConfigProperties dataConfigProperties() {
        return new DataConfigProperties();
    }

    @Produces
    @ApplicationScoped
    public Translator translator() {
        return Translator.NOOP;
    }

    @Produces
    @ApplicationScoped
    public LockService lockService() {
        return new SimpleLockService();
    }

    @Produces
    @ApplicationScoped
    public DataStore dataStore() {
        TimedCache<String, Object> cache = CacheUtil.newTimedCache(0);
        return new DataStore() {
            @Override public void put(String k, Object v) { cache.put(k, v); }
            @Override public void put(String k, Object v, long t) { cache.put(k, v, t); }
            @Override public <T> T get(String k) { return (T) cache.get(k); }
            @Override public <T> T getAndRemove(String k) { T t = get(k); cache.remove(k); return t; }
            @Override public <T> T get(String k, java.util.function.Supplier<T> s) {
                if (containsKey(k)) return get(k); T v = s.get(); put(k, v); return v;
            }
            @Override public void remove(String k) { cache.remove(k); }
            @Override public boolean containsKey(String k) { return cache.containsKey(k); }
        };
    }

    @Produces
    @ApplicationScoped
    public LogManager logManager() {
        return new LogManager();
    }

    @Produces
    @ApplicationScoped
    public RequestPolicy requestPolicy() {
        return new PurposeRequestPolicy();
    }
}
