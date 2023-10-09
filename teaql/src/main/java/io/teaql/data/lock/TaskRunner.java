package io.teaql.data.lock;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.stream.StreamUtil;
import cn.hutool.core.thread.ThreadUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.ObjUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.log.StaticLog;
import io.teaql.data.Entity;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

public class TaskRunner {

  public ConcurrentHashMap<String, Lock> locks = new ConcurrentHashMap<>();
  public Executor executor = ThreadUtil.newExecutorByBlockingCoefficient(0.5f);

  public void execute(Runnable runnable, Entity... entities) {
    List<String> list =
        StreamUtil.of(entities)
            .filter(entity -> entity != null)
            .map(entity -> entity.typeName() + entity.getId())
            .collect(Collectors.toList());
    String[] keys = ArrayUtil.toArray(list, String.class);
    execute(runnable, keys);
  }

  public void execute(Runnable runnable, String... keys) {
    lock(keys);
    try {
      runnable.run();
    } finally {
      unlock(keys);
    }
  }

  public void singleTaskRun(String taskName, Runnable runnable) {
    boolean canRun = tryLock(taskName);
    if (!canRun) {
      throw new RuntimeException(StrUtil.format("Task {} is already running.", taskName));
    }
    try {
      runnable.run();
    } finally {
      unlock(taskName);
    }
  }

  public void trySingleTaskRun(String taskName, Runnable runnable) {
    boolean canRun = tryLock(taskName);
    if (!canRun) {
      StaticLog.info("Task {} is already running.", taskName);
      return;
    }
    try {
      runnable.run();
    } finally {
      unlock(taskName);
    }
  }

  public void singleTaskRunASync(String taskName, Runnable runnable) {
    executor.execute(
        () -> {
          trySingleTaskRun(taskName, runnable);
        });
  }

  public <V> V call(Callable<V> callable, String... keys) {
    lock(keys);
    try {
      return callable.call();
    } catch (Exception pE) {
      throw new RuntimeException(pE);
    } finally {
      unlock(keys);
    }
  }

  private void lock(String... keys) {
    keys = ArrayUtil.removeNull(keys);
    if (ObjUtil.isEmpty(keys)) {
      return;
    }
    List<String> list = ListUtil.list(false, keys);
    Collections.sort(list);
    for (String key : list) {
      Lock lock = ensureLockForKey(key);
      lock.lock();
    }
  }

  private boolean tryLock(String... keys) {
    keys = ArrayUtil.removeNull(keys);
    if (ObjUtil.isEmpty(keys)) {
      return true;
    }
    List<String> list = ListUtil.list(false, keys);
    Collections.sort(list);
    for (String key : list) {
      Lock lock = ensureLockForKey(key);
      if (!lock.tryLock()) {
        return false;
      }
    }
    return true;
  }

  private void unlock(String... keys) {
    keys = ArrayUtil.removeNull(keys);
    if (ObjUtil.isEmpty(keys)) {
      return;
    }
    List<String> list = ListUtil.list(false, keys);
    Collections.sort(list);
    Collections.reverse(list);
    for (String key : list) {
      Lock lock = ensureLockForKey(key);
      lock.unlock();
    }
  }

  private Lock ensureLockForKey(String key) {
    ReentrantLock lock = new ReentrantLock();
    Lock l = locks.putIfAbsent(key, lock);
    if (l != null) {
      return l;
    }
    return lock;
  }
}
