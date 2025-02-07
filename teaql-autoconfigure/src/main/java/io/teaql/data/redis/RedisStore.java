package io.teaql.data.redis;

import io.teaql.data.DataStore;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import org.redisson.api.RedissonClient;

public class RedisStore implements DataStore {
  private RedissonClient redissonClient;

  public RedisStore(RedissonClient pRedissonClient) {
    redissonClient = pRedissonClient;
  }

  @Override
  public void put(String key, Object object) {
    redissonClient.getBucket(key).set(object);
  }

  @Override
  public void put(String key, Object object, long timeout) {
    redissonClient.getBucket(key).set(object, timeout, TimeUnit.SECONDS);
  }

  @Override
  public <T> T get(String key) {
    return (T) redissonClient.getBucket(key).get();
  }

  @Override
  public <T> T getAndRemove(String key) {
    return (T) redissonClient.getBucket(key).getAndDelete();
  }

  @Override
  public <T> T get(String key, Supplier<T> supplier) {
    if (containsKey(key)) {
      return get(key);
    }
    T v = supplier.get();
    redissonClient.getBucket(key).set(v);
    return v;
  }

  @Override
  public void remove(String key) {
    redissonClient.getBucket(key).delete();
  }

  @Override
  public boolean containsKey(String key) {
    return redissonClient.getBucket(key).isExists();
  }
}
