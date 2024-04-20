package io.teaql.data.redis;

import io.teaql.data.DataStore;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import org.springframework.data.redis.core.RedisTemplate;

public class RedisStore implements DataStore {
  private RedisTemplate<String, Object> redisTemplate;

  public RedisStore(RedisTemplate<String, Object> pRedisTemplate) {
    redisTemplate = pRedisTemplate;
  }

  @Override
  public void put(String key, Object object) {
    redisTemplate.opsForValue().set(key, object);
  }

  @Override
  public void put(String key, Object object, long timeout) {
    redisTemplate.opsForValue().set(key, object, timeout, TimeUnit.SECONDS);
  }

  @Override
  public <T> T get(String key) {
    return (T) redisTemplate.opsForValue().get(key);
  }

  @Override
  public <T> T getAndRemove(String key) {
    return (T) redisTemplate.opsForValue().getAndDelete(key);
  }

  @Override
  public <T> T get(String key, Supplier<T> supplier) {
    if (containsKey(key)) {
      return get(key);
    }
    T v = supplier.get();
    redisTemplate.opsForValue().set(key, v);
    return v;
  }

  @Override
  public void remove(String key) {
    redisTemplate.delete(key);
  }

  @Override
  public boolean containsKey(String key) {
    return redisTemplate.hasKey(key);
  }
}
