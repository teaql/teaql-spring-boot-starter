package io.teaql.data.utils;

public class ThreadUtil {

    public static java.util.concurrent.ThreadPoolExecutor newExecutorByBlockingCoefficient(float p0) {
        return cn.hutool.core.thread.ThreadUtil.newExecutorByBlockingCoefficient(p0);
    }

}
