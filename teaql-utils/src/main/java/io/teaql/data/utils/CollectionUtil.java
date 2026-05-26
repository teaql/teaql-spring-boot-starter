package io.teaql.data.utils;

public class CollectionUtil {

    public static boolean contains(java.util.Collection<?> p0, java.lang.Object p1) {
        return cn.hutool.core.collection.CollectionUtil.contains(p0, p1);
    }

    public static <T> boolean contains(java.util.Collection<T> p0, java.util.function.Predicate<? super T> p1) {
        return cn.hutool.core.collection.CollectionUtil.contains(p0, p1);
    }

    public static boolean isEmpty(java.lang.Iterable<?> p0) {
        return cn.hutool.core.collection.CollectionUtil.isEmpty(p0);
    }

    public static boolean isEmpty(java.util.Collection<?> p0) {
        return cn.hutool.core.collection.CollectionUtil.isEmpty(p0);
    }

    public static boolean isEmpty(java.util.Enumeration<?> p0) {
        return cn.hutool.core.collection.CollectionUtil.isEmpty(p0);
    }

    public static boolean isEmpty(java.util.Iterator<?> p0) {
        return cn.hutool.core.collection.CollectionUtil.isEmpty(p0);
    }

    public static boolean isEmpty(java.util.Map<?, ?> p0) {
        return cn.hutool.core.collection.CollectionUtil.isEmpty(p0);
    }

    public static int size(java.lang.Object p0) {
        return cn.hutool.core.collection.CollectionUtil.size(p0);
    }

    public static <T> T findOne(java.lang.Iterable<T> p0, cn.hutool.core.lang.Filter<T> p1) {
        return cn.hutool.core.collection.CollectionUtil.findOne(p0, p1);
    }

    public static <T> T get(java.util.Collection<T> p0, int p1) {
        return cn.hutool.core.collection.CollectionUtil.get(p0, p1);
    }

    public static <T> T getFirst(java.lang.Iterable<T> p0) {
        return cn.hutool.core.collection.CollectionUtil.getFirst(p0);
    }

    public static <T> T getFirst(java.util.Iterator<T> p0) {
        return cn.hutool.core.collection.CollectionUtil.getFirst(p0);
    }

    public static <T> T getLast(java.util.Collection<T> p0) {
        return cn.hutool.core.collection.CollectionUtil.getLast(p0);
    }

    public static <T> java.lang.String join(java.lang.Iterable<T> p0, java.lang.CharSequence p1) {
        return cn.hutool.core.collection.CollectionUtil.join(p0, p1);
    }

    public static <T> java.lang.String join(java.lang.Iterable<T> p0, java.lang.CharSequence p1, java.lang.String p2, java.lang.String p3) {
        return cn.hutool.core.collection.CollectionUtil.join(p0, p1, p2, p3);
    }

    public static <T> java.lang.String join(java.lang.Iterable<T> p0, java.lang.CharSequence p1, java.util.function.Function<T, ? extends java.lang.CharSequence> p2) {
        return cn.hutool.core.collection.CollectionUtil.join(p0, p1, p2);
    }

    public static <T> java.lang.String join(java.util.Iterator<T> p0, java.lang.CharSequence p1) {
        return cn.hutool.core.collection.CollectionUtil.join(p0, p1);
    }

    public static <T> java.util.Collection<T> subtract(java.util.Collection<T> p0, java.util.Collection<T> p1) {
        return cn.hutool.core.collection.CollectionUtil.subtract(p0, p1);
    }

    public static <T, R> java.util.List<R> map(java.lang.Iterable<T> p0, java.util.function.Function<? super T, ? extends R> p1, boolean p2) {
        return cn.hutool.core.collection.CollectionUtil.map(p0, p1, p2);
    }

    public static <T> java.util.List<T> sub(java.util.Collection<T> p0, int p1, int p2) {
        return cn.hutool.core.collection.CollectionUtil.sub(p0, p1, p2);
    }

    public static <T> java.util.List<T> sub(java.util.Collection<T> p0, int p1, int p2, int p3) {
        return cn.hutool.core.collection.CollectionUtil.sub(p0, p1, p2, p3);
    }

    public static <T> java.util.List<T> sub(java.util.List<T> p0, int p1, int p2) {
        return cn.hutool.core.collection.CollectionUtil.sub(p0, p1, p2);
    }

    public static <T> java.util.List<T> sub(java.util.List<T> p0, int p1, int p2, int p3) {
        return cn.hutool.core.collection.CollectionUtil.sub(p0, p1, p2, p3);
    }

}
