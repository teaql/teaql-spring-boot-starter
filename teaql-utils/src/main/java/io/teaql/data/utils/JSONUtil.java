package io.teaql.data.utils;

public class JSONUtil {

    public static cn.hutool.json.JSONObject parseObj(java.lang.Object p0) {
        return cn.hutool.json.JSONUtil.parseObj(p0);
    }

    public static cn.hutool.json.JSONObject parseObj(java.lang.Object p0, boolean p1) {
        return cn.hutool.json.JSONUtil.parseObj(p0, p1);
    }

    public static cn.hutool.json.JSONObject parseObj(java.lang.Object p0, boolean p1, boolean p2) {
        return cn.hutool.json.JSONUtil.parseObj(p0, p1, p2);
    }

    public static cn.hutool.json.JSONObject parseObj(java.lang.Object p0, cn.hutool.json.JSONConfig p1) {
        return cn.hutool.json.JSONUtil.parseObj(p0, p1);
    }

    public static cn.hutool.json.JSONObject parseObj(java.lang.String p0) {
        return cn.hutool.json.JSONUtil.parseObj(p0);
    }

    public static <T> T toBean(cn.hutool.json.JSON p0, io.teaql.data.utils.TypeReference<T> p1, boolean p2) {
        return cn.hutool.json.JSONUtil.toBean(p0, p1.getType(), p2);
    }

    public static <T> T toBean(cn.hutool.json.JSON p0, java.lang.reflect.Type p1, boolean p2) {
        return cn.hutool.json.JSONUtil.toBean(p0, p1, p2);
    }

    public static <T> T toBean(cn.hutool.json.JSONObject p0, java.lang.Class<T> p1) {
        return cn.hutool.json.JSONUtil.toBean(p0, p1);
    }

    public static <T> T toBean(java.lang.String p0, io.teaql.data.utils.TypeReference<T> p1, boolean p2) {
        return cn.hutool.json.JSONUtil.toBean(p0, p1.getType(), p2);
    }

    public static <T> T toBean(java.lang.String p0, cn.hutool.json.JSONConfig p1, java.lang.Class<T> p2) {
        return cn.hutool.json.JSONUtil.toBean(p0, p1, p2);
    }

    public static <T> T toBean(java.lang.String p0, java.lang.Class<T> p1) {
        return cn.hutool.json.JSONUtil.toBean(p0, p1);
    }

    public static <T> T toBean(java.lang.String p0, java.lang.reflect.Type p1, boolean p2) {
        return cn.hutool.json.JSONUtil.toBean(p0, p1, p2);
    }

    public static java.lang.String toJsonStr(cn.hutool.json.JSON p0) {
        return cn.hutool.json.JSONUtil.toJsonStr(p0);
    }

    public static java.lang.String toJsonStr(cn.hutool.json.JSON p0, int p1) {
        return cn.hutool.json.JSONUtil.toJsonStr(p0, p1);
    }

    public static java.lang.String toJsonStr(java.lang.Object p0) {
        return cn.hutool.json.JSONUtil.toJsonStr(p0);
    }

    public static java.lang.String toJsonStr(java.lang.Object p0, cn.hutool.json.JSONConfig p1) {
        return cn.hutool.json.JSONUtil.toJsonStr(p0, p1);
    }

    public static void toJsonStr(cn.hutool.json.JSON p0, java.io.Writer p1) {
        cn.hutool.json.JSONUtil.toJsonStr(p0, p1);
    }

    public static void toJsonStr(java.lang.Object p0, java.io.Writer p1) {
        cn.hutool.json.JSONUtil.toJsonStr(p0, p1);
    }

}
