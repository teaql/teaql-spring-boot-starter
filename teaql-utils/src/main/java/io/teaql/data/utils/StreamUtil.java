package io.teaql.data.utils;

public class StreamUtil {

    public static java.util.stream.Stream<java.lang.String> of(java.io.File p0) {
        return cn.hutool.core.stream.StreamUtil.of(p0);
    }

    public static java.util.stream.Stream<java.lang.String> of(java.io.File p0, java.nio.charset.Charset p1) {
        return cn.hutool.core.stream.StreamUtil.of(p0, p1);
    }

    public static <T> java.util.stream.Stream<T> of(java.lang.Iterable<T> p0) {
        return cn.hutool.core.stream.StreamUtil.of(p0);
    }

    public static <T> java.util.stream.Stream<T> of(java.lang.Iterable<T> p0, boolean p1) {
        return cn.hutool.core.stream.StreamUtil.of(p0, p1);
    }

    public static <T> java.util.stream.Stream<T> of(T p0, java.util.function.UnaryOperator<T> p1, int p2) {
        return cn.hutool.core.stream.StreamUtil.of(p0, p1, p2);
    }

    public static <T> java.util.stream.Stream<T> of(T... p0) {
        return cn.hutool.core.stream.StreamUtil.of(p0);
    }

    public static java.util.stream.Stream<java.lang.String> of(java.nio.file.Path p0) {
        return cn.hutool.core.stream.StreamUtil.of(p0);
    }

    public static java.util.stream.Stream<java.lang.String> of(java.nio.file.Path p0, java.nio.charset.Charset p1) {
        return cn.hutool.core.stream.StreamUtil.of(p0, p1);
    }

    public static <T> java.util.stream.Stream<T> of(java.util.Iterator<T> p0) {
        return cn.hutool.core.stream.StreamUtil.of(p0);
    }

    public static <T> java.util.stream.Stream<T> of(java.util.Iterator<T> p0, boolean p1) {
        return cn.hutool.core.stream.StreamUtil.of(p0, p1);
    }

}
