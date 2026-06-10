package io.teaql.data.micronaut;

import javax.sql.DataSource;

import io.micronaut.context.annotation.Bean;
import io.micronaut.context.annotation.Factory;
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
 * Micronaut Bean Factory。为 TeaQL 运行时提供默认 Bean。
 * 应用层可以 @Override 任何 Bean。
 */
@Factory
public class TeaQLFactory {

    @Bean
    @Singleton
    public TeaQLDatabase teaQLDatabase(DataSource dataSource) {
        return new MicronautDatabase(dataSource);
    }

    @Bean
    @Singleton
    public EntityMetaFactory entityMetaFactory() {
        return new SimpleEntityMetaFactory();
    }

    @Bean
    @Singleton
    public DataConfigProperties dataConfigProperties() {
        return new DataConfigProperties();
    }

    @Bean
    @Singleton
    public Translator translator() {
        return Translator.NOOP;
    }

    @Bean
    @Singleton
    public LockService lockService() {
        return new SimpleLockService();
    }

    @Bean
    @Singleton
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

    @Bean
    @Singleton
    public LogManager logManager() {
        return new LogManager();
    }

    @Bean
    @Singleton
    public RequestPolicy requestPolicy() {
        return new PurposeRequestPolicy();
    }
}
