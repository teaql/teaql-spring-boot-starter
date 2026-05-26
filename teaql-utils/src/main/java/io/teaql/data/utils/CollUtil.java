package io.teaql.data.utils;

public class CollUtil {

    public static <T> T get(java.util.Collection<T> p0, int p1) {
        return cn.hutool.core.collection.CollUtil.get(p0, p1);
    }

    public static <T> T getFirst(java.lang.Iterable<T> p0) {
        return cn.hutool.core.collection.CollUtil.getFirst(p0);
    }

    public static <T> T getFirst(java.util.Iterator<T> p0) {
        return cn.hutool.core.collection.CollUtil.getFirst(p0);
    }

    public static <T> java.util.Collection<T> filterNew(java.util.Collection<T> p0, cn.hutool.core.lang.Filter<T> p1) {
        return cn.hutool.core.collection.CollUtil.filterNew(p0, p1);
    }

}
