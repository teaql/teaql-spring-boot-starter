package io.teaql.data.utils;

public class ObjectUtil {

    public static boolean equals(java.lang.Object p0, java.lang.Object p1) {
        return cn.hutool.core.util.ObjectUtil.equals(p0, p1);
    }

    public static boolean isEmpty(java.lang.Object p0) {
        return cn.hutool.core.util.ObjectUtil.isEmpty(p0);
    }

    public static boolean isNotEmpty(java.lang.Object p0) {
        return cn.hutool.core.util.ObjectUtil.isNotEmpty(p0);
    }

    public static boolean isNotNull(java.lang.Object p0) {
        return cn.hutool.core.util.ObjectUtil.isNotNull(p0);
    }

    public static boolean isNull(java.lang.Object p0) {
        return cn.hutool.core.util.ObjectUtil.isNull(p0);
    }

    public static <T extends java.lang.Comparable<? super T>> int compare(T p0, T p1) {
        return cn.hutool.core.util.ObjectUtil.compare(p0, p1);
    }

    public static <T extends java.lang.Comparable<? super T>> int compare(T p0, T p1, boolean p2) {
        return cn.hutool.core.util.ObjectUtil.compare(p0, p1, p2);
    }

    public static int length(java.lang.Object p0) {
        return cn.hutool.core.util.ObjectUtil.length(p0);
    }

}
