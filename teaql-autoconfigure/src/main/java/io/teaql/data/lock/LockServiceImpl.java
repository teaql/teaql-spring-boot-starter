package io.teaql.data.lock;

import io.teaql.data.UserContext;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import org.redisson.api.RedissonClient;

public class LockServiceImpl implements LockService {
  private Map<String, Lock> localLocks = new ConcurrentHashMap<>();

  @Override
  public Lock getLocalLock(UserContext ctx, String key) {
    return localLocks.computeIfAbsent(key, k -> new LockWrapper(k, new ReentrantLock()));
  }

  @Override
  public Lock getDistributeLock(UserContext ctx, String key) {
    RedissonClient client = ctx.getBean(RedissonClient.class);
    return client.getLock(key);
  }

  class LockWrapper implements Lock {
    private Lock lock;
    private String key;

    public LockWrapper(String key, Lock lock) {
      this.key = key;
      this.lock = lock;
    }

    @Override
    public void lock() {
      lock.lock();
    }

    @Override
    public void lockInterruptibly() throws InterruptedException {
      lock.lockInterruptibly();
    }

    @Override
    public boolean tryLock() {
      return lock.tryLock();
    }

    @Override
    public boolean tryLock(long time, TimeUnit unit) throws InterruptedException {
      return lock.tryLock(time, unit);
    }

    @Override
    public void unlock() {
      try {
        lock.unlock();
      } finally {
        localLocks.remove(key);
      }
    }

    @Override
    public Condition newCondition() {
      return lock.newCondition();
    }
  }
}
