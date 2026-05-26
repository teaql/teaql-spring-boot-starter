package io.teaql.data.utils;

public class HttpUtil {

    public static java.lang.String post(java.lang.String p0, java.lang.String p1) {
        return cn.hutool.http.HttpUtil.post(p0, p1);
    }

    public static java.lang.String post(java.lang.String p0, java.lang.String p1, int p2) {
        return cn.hutool.http.HttpUtil.post(p0, p1, p2);
    }

    public static java.lang.String post(java.lang.String p0, java.util.Map<java.lang.String, java.lang.Object> p1) {
        return cn.hutool.http.HttpUtil.post(p0, p1);
    }

    public static java.lang.String post(java.lang.String p0, java.util.Map<java.lang.String, java.lang.Object> p1, int p2) {
        return cn.hutool.http.HttpUtil.post(p0, p1, p2);
    }

}
