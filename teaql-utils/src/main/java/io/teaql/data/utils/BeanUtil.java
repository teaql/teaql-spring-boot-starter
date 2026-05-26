package io.teaql.data.utils;

public class BeanUtil {

    public static <T> T getProperty(java.lang.Object p0, java.lang.String p1) {
        return cn.hutool.core.bean.BeanUtil.getProperty(p0, p1);
    }

    public static <T> T toBean(java.lang.Class<T> p0, cn.hutool.core.bean.copier.ValueProvider<java.lang.String> p1, cn.hutool.core.bean.copier.CopyOptions p2) {
        return cn.hutool.core.bean.BeanUtil.toBean(p0, p1, p2);
    }

    public static <T> T toBean(java.lang.Object p0, java.lang.Class<T> p1) {
        return cn.hutool.core.bean.BeanUtil.toBean(p0, p1);
    }

    public static <T> T toBean(java.lang.Object p0, java.lang.Class<T> p1, cn.hutool.core.bean.copier.CopyOptions p2) {
        return cn.hutool.core.bean.BeanUtil.toBean(p0, p1, p2);
    }

    public static <T> T toBean(java.lang.Object p0, java.util.function.Supplier<T> p1, cn.hutool.core.bean.copier.CopyOptions p2) {
        return cn.hutool.core.bean.BeanUtil.toBean(p0, p1, p2);
    }

    public static java.util.Map<java.lang.String, java.lang.Object> beanToMap(java.lang.Object p0, boolean p1, boolean p2) {
        return cn.hutool.core.bean.BeanUtil.beanToMap(p0, p1, p2);
    }

    public static java.util.Map<java.lang.String, java.lang.Object> beanToMap(java.lang.Object p0, java.lang.String... p1) {
        return cn.hutool.core.bean.BeanUtil.beanToMap(p0, p1);
    }

    public static java.util.Map<java.lang.String, java.lang.Object> beanToMap(java.lang.Object p0, java.util.Map<java.lang.String, java.lang.Object> p1, boolean p2, boolean p3) {
        return cn.hutool.core.bean.BeanUtil.beanToMap(p0, p1, p2, p3);
    }

    public static java.util.Map<java.lang.String, java.lang.Object> beanToMap(java.lang.Object p0, java.util.Map<java.lang.String, java.lang.Object> p1, boolean p2, cn.hutool.core.lang.Editor<java.lang.String> p3) {
        return cn.hutool.core.bean.BeanUtil.beanToMap(p0, p1, p2, p3);
    }

    public static java.util.Map<java.lang.String, java.lang.Object> beanToMap(java.lang.Object p0, java.util.Map<java.lang.String, java.lang.Object> p1, cn.hutool.core.bean.copier.CopyOptions p2) {
        return cn.hutool.core.bean.BeanUtil.beanToMap(p0, p1, p2);
    }

    public static void setProperty(java.lang.Object p0, java.lang.String p1, java.lang.Object p2) {
        cn.hutool.core.bean.BeanUtil.setProperty(p0, p1, p2);
    }

}
