package io.teaql.data.utils;

public class URLDecoder {

    public static byte[] decode(byte[] p0) {
        return cn.hutool.core.net.URLDecoder.decode(p0);
    }

    public static byte[] decode(byte[] p0, boolean p1) {
        return cn.hutool.core.net.URLDecoder.decode(p0, p1);
    }

    public static java.lang.String decode(java.lang.String p0, java.nio.charset.Charset p1) {
        return cn.hutool.core.net.URLDecoder.decode(p0, p1);
    }

    public static java.lang.String decode(java.lang.String p0, java.nio.charset.Charset p1, boolean p2) {
        return cn.hutool.core.net.URLDecoder.decode(p0, p1, p2);
    }

}
