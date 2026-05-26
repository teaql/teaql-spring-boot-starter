package io.teaql.data.utils;

public class CallerUtil {

    public static java.lang.Class<?> getCaller() {
        return cn.hutool.core.lang.caller.CallerUtil.getCaller();
    }

    public static java.lang.Class<?> getCaller(int p0) {
        return cn.hutool.core.lang.caller.CallerUtil.getCaller(p0);
    }

}
