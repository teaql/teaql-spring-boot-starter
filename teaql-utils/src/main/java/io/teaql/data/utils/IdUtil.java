package io.teaql.data.utils;

public class IdUtil {

    public static java.lang.String fastSimpleUUID() {
        return cn.hutool.core.util.IdUtil.fastSimpleUUID();
    }

    public static java.lang.String getSnowflakeNextIdStr() {
        return cn.hutool.core.util.IdUtil.getSnowflakeNextIdStr();
    }

    public static long getSnowflakeNextId() {
        return cn.hutool.core.util.IdUtil.getSnowflakeNextId();
    }

}
