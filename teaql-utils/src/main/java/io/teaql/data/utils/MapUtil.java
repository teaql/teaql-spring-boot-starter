package io.teaql.data.utils;

public class MapUtil {

    public static <K, V> cn.hutool.core.map.MapBuilder<K, V> builder() {
        return cn.hutool.core.map.MapUtil.builder();
    }

    public static <K, V> cn.hutool.core.map.MapBuilder<K, V> builder(K p0, V p1) {
        return cn.hutool.core.map.MapUtil.builder(p0, p1);
    }

    public static <K, V> cn.hutool.core.map.MapBuilder<K, V> builder(java.util.Map<K, V> p0) {
        return cn.hutool.core.map.MapUtil.builder(p0);
    }

    public static java.lang.Boolean getBool(java.util.Map<?, ?> p0, java.lang.Object p1) {
        return cn.hutool.core.map.MapUtil.getBool(p0, p1);
    }

    public static java.lang.Boolean getBool(java.util.Map<?, ?> p0, java.lang.Object p1, java.lang.Boolean p2) {
        return cn.hutool.core.map.MapUtil.getBool(p0, p1, p2);
    }

    public static <K, V> java.lang.String joinIgnoreNull(java.util.Map<K, V> p0, java.lang.String p1, java.lang.String p2, java.lang.String... p3) {
        return cn.hutool.core.map.MapUtil.joinIgnoreNull(p0, p1, p2, p3);
    }

    public static <K, V> java.util.HashMap<K, V> of(K p0, V p1) {
        return cn.hutool.core.map.MapUtil.of(p0, p1);
    }

    public static <K, V> java.util.HashMap<K, V> of(K p0, V p1, boolean p2) {
        return cn.hutool.core.map.MapUtil.of(p0, p1, p2);
    }

    public static java.util.HashMap<java.lang.Object, java.lang.Object> of(java.lang.Object[] p0) {
        return cn.hutool.core.map.MapUtil.of(p0);
    }

    public static <K, V> java.util.Map<K, V> createMap(java.lang.Class<?> p0) {
        return cn.hutool.core.map.MapUtil.createMap(p0);
    }

    public static <K, V> java.util.Map<K, V> empty() {
        return cn.hutool.core.map.MapUtil.empty();
    }

    public static <K, V, T extends java.util.Map<K, V>> T empty(java.lang.Class<?> p0) {
        return cn.hutool.core.map.MapUtil.empty(p0);
    }

    public static <K, V> java.util.Map<K, V> of(cn.hutool.core.lang.Pair<K, V>... p0) {
        return cn.hutool.core.map.MapUtil.of(p0);
    }

    public static <K, V> java.util.TreeMap<K, V> sort(java.util.Map<K, V> p0) {
        return cn.hutool.core.map.MapUtil.sort(p0);
    }

    public static <K, V> java.util.TreeMap<K, V> sort(java.util.Map<K, V> p0, java.util.Comparator<? super K> p1) {
        return cn.hutool.core.map.MapUtil.sort(p0, p1);
    }

}
