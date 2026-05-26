package io.teaql.data.utils;

public class ListUtil {

    public static <T> java.util.ArrayList<T> toList(java.lang.Iterable<T> p0) {
        return cn.hutool.core.collection.ListUtil.toList(p0);
    }

    public static <T> java.util.ArrayList<T> toList(T... p0) {
        return cn.hutool.core.collection.ListUtil.toList(p0);
    }

    public static <T> java.util.ArrayList<T> toList(java.util.Collection<T> p0) {
        return cn.hutool.core.collection.ListUtil.toList(p0);
    }

    public static <T> java.util.ArrayList<T> toList(java.util.Enumeration<T> p0) {
        return cn.hutool.core.collection.ListUtil.toList(p0);
    }

    public static <T> java.util.ArrayList<T> toList(java.util.Iterator<T> p0) {
        return cn.hutool.core.collection.ListUtil.toList(p0);
    }

    public static <T> java.util.List<T> empty() {
        return cn.hutool.core.collection.ListUtil.empty();
    }

    public static <T> java.util.List<T> list(boolean p0) {
        return cn.hutool.core.collection.ListUtil.list(p0);
    }

    public static <T> java.util.List<T> list(boolean p0, java.lang.Iterable<T> p1) {
        return cn.hutool.core.collection.ListUtil.list(p0, p1);
    }

    public static <T> java.util.List<T> list(boolean p0, T... p1) {
        return cn.hutool.core.collection.ListUtil.list(p0, p1);
    }

    public static <T> java.util.List<T> list(boolean p0, java.util.Collection<T> p1) {
        return cn.hutool.core.collection.ListUtil.list(p0, p1);
    }

    public static <T> java.util.List<T> list(boolean p0, java.util.Enumeration<T> p1) {
        return cn.hutool.core.collection.ListUtil.list(p0, p1);
    }

    public static <T> java.util.List<T> list(boolean p0, java.util.Iterator<T> p1) {
        return cn.hutool.core.collection.ListUtil.list(p0, p1);
    }

    public static <T> java.util.List<T> of(T... p0) {
        return cn.hutool.core.collection.ListUtil.of(p0);
    }

}
