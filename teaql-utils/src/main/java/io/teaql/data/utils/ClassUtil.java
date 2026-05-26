package io.teaql.data.utils;

public class ClassUtil {

    public static boolean isAbstract(java.lang.Class<?> p0) {
        return cn.hutool.core.util.ClassUtil.isAbstract(p0);
    }

    public static boolean isAssignable(java.lang.Class<?> p0, java.lang.Class<?> p1) {
        return cn.hutool.core.util.ClassUtil.isAssignable(p0, p1);
    }

    public static boolean isInterface(java.lang.Class<?> p0) {
        return cn.hutool.core.util.ClassUtil.isInterface(p0);
    }

    public static boolean isSimpleValueType(java.lang.Class<?> p0) {
        return cn.hutool.core.util.ClassUtil.isSimpleValueType(p0);
    }

    public static <T> java.lang.Class<T> loadClass(java.lang.String p0) {
        return cn.hutool.core.util.ClassUtil.loadClass(p0);
    }

    public static <T> java.lang.Class<T> loadClass(java.lang.String p0, boolean p1) {
        return cn.hutool.core.util.ClassUtil.loadClass(p0, p1);
    }

    public static java.lang.reflect.Method[] getPublicMethods(java.lang.Class<?> p0) {
        return cn.hutool.core.util.ClassUtil.getPublicMethods(p0);
    }

    public static java.util.List<java.lang.reflect.Method> getPublicMethods(java.lang.Class<?> p0, cn.hutool.core.lang.Filter<java.lang.reflect.Method> p1) {
        return cn.hutool.core.util.ClassUtil.getPublicMethods(p0, p1);
    }

    public static java.util.List<java.lang.reflect.Method> getPublicMethods(java.lang.Class<?> p0, java.lang.String... p1) {
        return cn.hutool.core.util.ClassUtil.getPublicMethods(p0, p1);
    }

    public static java.util.List<java.lang.reflect.Method> getPublicMethods(java.lang.Class<?> p0, java.lang.reflect.Method... p1) {
        return cn.hutool.core.util.ClassUtil.getPublicMethods(p0, p1);
    }

    public static java.util.Set<java.lang.Class<?>> scanPackageBySuper(java.lang.String p0, java.lang.Class<?> p1) {
        return cn.hutool.core.util.ClassUtil.scanPackageBySuper(p0, p1);
    }

}
