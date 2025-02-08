package io.teaql.data.lock;

import cn.hutool.core.thread.ThreadUtil;
import io.teaql.data.UserContext;
import java.util.concurrent.Executor;
import java.util.concurrent.locks.Lock;

public interface LockService {
  Executor taskExecutor = ThreadUtil.newExecutorByBlockingCoefficient(0.5f);

  Lock getLocalLock(UserContext ctx, String key);

  Lock getDistributeLock(UserContext ctx, String key);
}
