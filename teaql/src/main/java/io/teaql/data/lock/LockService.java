package io.teaql.data.lock;

import java.util.concurrent.Executor;
import java.util.concurrent.locks.Lock;

import io.teaql.data.utils.ThreadUtil;

import io.teaql.data.UserContext;

public interface LockService {
    Executor taskExecutor = ThreadUtil.newExecutorByBlockingCoefficient(0.5f);

    Lock getLocalLock(UserContext ctx, String key);

    Lock getDistributeLock(UserContext ctx, String key);
}
