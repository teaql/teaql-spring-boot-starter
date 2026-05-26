package io.teaql.data.utils;

public class TemporalAccessorUtil {

    public static long toEpochMilli(java.time.temporal.TemporalAccessor p0) {
        return cn.hutool.core.date.TemporalAccessorUtil.toEpochMilli(p0);
    }

}
