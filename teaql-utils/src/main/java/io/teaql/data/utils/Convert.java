package io.teaql.data.utils;

public class Convert {

    public static <T> T convert(io.teaql.data.utils.TypeReference<T> p0, java.lang.Object p1) {
        return cn.hutool.core.convert.Convert.convert(p0.getType(), p1);
    }

    public static <T> T convert(java.lang.Class<T> p0, java.lang.Object p1) {
        return cn.hutool.core.convert.Convert.convert(p0, p1);
    }

    public static <T> T convert(java.lang.Class<T> p0, java.lang.Object p1, T p2) {
        return cn.hutool.core.convert.Convert.convert(p0, p1, p2);
    }

    public static <T> T convert(java.lang.reflect.Type p0, java.lang.Object p1) {
        return cn.hutool.core.convert.Convert.convert(p0, p1);
    }

    public static <T> T convert(java.lang.reflect.Type p0, java.lang.Object p1, T p2) {
        return cn.hutool.core.convert.Convert.convert(p0, p1, p2);
    }

}
