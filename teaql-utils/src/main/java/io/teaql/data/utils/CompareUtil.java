package io.teaql.data.utils;

public class CompareUtil {

    public static <T extends java.lang.Comparable<? super T>> int compare(T p0, T p1) {
        return cn.hutool.core.comparator.CompareUtil.compare(p0, p1);
    }

    public static <T extends java.lang.Comparable<? super T>> int compare(T p0, T p1, boolean p2) {
        return cn.hutool.core.comparator.CompareUtil.compare(p0, p1, p2);
    }

    public static <T> int compare(T p0, T p1, boolean p2) {
        return cn.hutool.core.comparator.CompareUtil.compare(p0, p1, p2);
    }

    public static <T> int compare(T p0, T p1, java.util.Comparator<T> p2) {
        return cn.hutool.core.comparator.CompareUtil.compare(p0, p1, p2);
    }

}
