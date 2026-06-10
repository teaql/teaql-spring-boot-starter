package io.teaql.data.lock;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import io.teaql.data.UserContext;

/**
 * 简单的本地锁服务实现。不依赖 Spring。
 * 用于 Quarkus / Micronaut / 纯 Java 环境。
 */
public class SimpleLockService implements LockService {

    private final ConcurrentHashMap<String, ReentrantLock> locks = new ConcurrentHashMap<>();

    @Override
    public Lock getLocalLock(UserContext ctx, String lockName) {
        return locks.computeIfAbsent(lockName, k -> new ReentrantLock());
    }

    @Override
    public Lock getDistributeLock(UserContext ctx, String lockName) {
        // 非分布式环境退化为本地锁
        return getLocalLock(ctx, lockName);
    }
}
