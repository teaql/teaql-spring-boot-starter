package io.teaql.data.utils;

public class ReflectUtil {

    public static java.lang.Object getStaticFieldValue(java.lang.reflect.Field p0) {
        return cn.hutool.core.util.ReflectUtil.getStaticFieldValue(p0);
    }

    public static <T> T invoke(java.lang.Object p0, java.lang.String p1, java.lang.Object... p2) {
        return cn.hutool.core.util.ReflectUtil.invoke(p0, p1, p2);
    }

    public static <T> T invoke(java.lang.Object p0, java.lang.reflect.Method p1, java.lang.Object... p2) {
        return cn.hutool.core.util.ReflectUtil.invoke(p0, p1, p2);
    }

    public static <T> T invokeStatic(java.lang.reflect.Method p0, java.lang.Object... p1) {
        return cn.hutool.core.util.ReflectUtil.invokeStatic(p0, p1);
    }

    public static <T> T newInstance(java.lang.Class<T> p0, java.lang.Object... p1) {
        return cn.hutool.core.util.ReflectUtil.newInstance(p0, p1);
    }

    public static <T> T newInstance(java.lang.String p0) {
        return cn.hutool.core.util.ReflectUtil.newInstance(p0);
    }

    public static <T> T newInstanceIfPossible(java.lang.Class<T> p0) {
        return cn.hutool.core.util.ReflectUtil.newInstanceIfPossible(p0);
    }

    public static java.lang.reflect.Field getField(java.lang.Class<?> p0, java.lang.String p1) {
        return cn.hutool.core.util.ReflectUtil.getField(p0, p1);
    }

    public static java.lang.reflect.Method getMethodByName(java.lang.Class<?> p0, boolean p1, java.lang.String p2) {
        return cn.hutool.core.util.ReflectUtil.getMethodByName(p0, p1, p2);
    }

    public static java.lang.reflect.Method getMethodByName(java.lang.Class<?> p0, java.lang.String p1) {
        return cn.hutool.core.util.ReflectUtil.getMethodByName(p0, p1);
    }

    public static java.lang.reflect.Method getMethodOfObj(java.lang.Object p0, java.lang.String p1, java.lang.Object... p2) {
        return cn.hutool.core.util.ReflectUtil.getMethodOfObj(p0, p1, p2);
    }

    public static java.lang.reflect.Method getPublicMethod(java.lang.Class<?> p0, java.lang.String p1, java.lang.Class<?>... p2) {
        return cn.hutool.core.util.ReflectUtil.getPublicMethod(p0, p1, p2);
    }

}
