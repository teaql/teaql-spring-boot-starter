package io.teaql.data.utils;

public class CollStreamUtil {

    public static <E, T> java.util.List<T> toList(java.util.Collection<E> p0, java.util.function.Function<E, T> p1) {
        return cn.hutool.core.collection.CollStreamUtil.toList(p0, p1);
    }

    public static <E, T> java.util.List<T> toList(java.util.Collection<E> p0, java.util.function.Function<E, T> p1, boolean p2) {
        return cn.hutool.core.collection.CollStreamUtil.toList(p0, p1, p2);
    }

    public static <E, K> java.util.Map<K, java.util.List<E>> groupByKey(java.util.Collection<E> p0, java.util.function.Function<E, K> p1) {
        return cn.hutool.core.collection.CollStreamUtil.groupByKey(p0, p1);
    }

    public static <E, K> java.util.Map<K, java.util.List<E>> groupByKey(java.util.Collection<E> p0, java.util.function.Function<E, K> p1, boolean p2) {
        return cn.hutool.core.collection.CollStreamUtil.groupByKey(p0, p1, p2);
    }

    public static <V, K> java.util.Map<K, V> toIdentityMap(java.util.Collection<V> p0, java.util.function.Function<V, K> p1) {
        return cn.hutool.core.collection.CollStreamUtil.toIdentityMap(p0, p1);
    }

    public static <V, K> java.util.Map<K, V> toIdentityMap(java.util.Collection<V> p0, java.util.function.Function<V, K> p1, boolean p2) {
        return cn.hutool.core.collection.CollStreamUtil.toIdentityMap(p0, p1, p2);
    }

    public static <E, K, V> java.util.Map<K, V> toMap(java.util.Collection<E> p0, java.util.function.Function<E, K> p1, java.util.function.Function<E, V> p2) {
        return cn.hutool.core.collection.CollStreamUtil.toMap(p0, p1, p2);
    }

    public static <E, K, V> java.util.Map<K, V> toMap(java.util.Collection<E> p0, java.util.function.Function<E, K> p1, java.util.function.Function<E, V> p2, boolean p3) {
        return cn.hutool.core.collection.CollStreamUtil.toMap(p0, p1, p2, p3);
    }

    public static <E, T> java.util.Set<T> toSet(java.util.Collection<E> p0, java.util.function.Function<E, T> p1) {
        return cn.hutool.core.collection.CollStreamUtil.toSet(p0, p1);
    }

    public static <E, T> java.util.Set<T> toSet(java.util.Collection<E> p0, java.util.function.Function<E, T> p1, boolean p2) {
        return cn.hutool.core.collection.CollStreamUtil.toSet(p0, p1, p2);
    }

}
