package io.teaql.data.utils;

public class Base64 {

    public static byte[] decode(byte[] p0) {
        return cn.hutool.core.codec.Base64.decode(p0);
    }

    public static byte[] decode(java.lang.CharSequence p0) {
        return cn.hutool.core.codec.Base64.decode(p0);
    }

    public static byte[] encode(byte[] p0, boolean p1) {
        return cn.hutool.core.codec.Base64.encode(p0, p1);
    }

    public static byte[] encode(byte[] p0, boolean p1, boolean p2) {
        return cn.hutool.core.codec.Base64.encode(p0, p1, p2);
    }

    public static java.lang.String decodeStr(java.lang.CharSequence p0) {
        return cn.hutool.core.codec.Base64.decodeStr(p0);
    }

    public static java.lang.String decodeStr(java.lang.CharSequence p0, java.lang.String p1) {
        return cn.hutool.core.codec.Base64.decodeStr(p0, p1);
    }

    public static java.lang.String decodeStr(java.lang.CharSequence p0, java.nio.charset.Charset p1) {
        return cn.hutool.core.codec.Base64.decodeStr(p0, p1);
    }

    public static java.lang.String encode(byte[] p0) {
        return cn.hutool.core.codec.Base64.encode(p0);
    }

    public static java.lang.String encode(java.io.File p0) {
        return cn.hutool.core.codec.Base64.encode(p0);
    }

    public static java.lang.String encode(java.io.InputStream p0) {
        return cn.hutool.core.codec.Base64.encode(p0);
    }

    public static java.lang.String encode(java.lang.CharSequence p0) {
        return cn.hutool.core.codec.Base64.encode(p0);
    }

    public static java.lang.String encode(java.lang.CharSequence p0, java.lang.String p1) {
        return cn.hutool.core.codec.Base64.encode(p0, p1);
    }

    public static java.lang.String encode(java.lang.CharSequence p0, java.nio.charset.Charset p1) {
        return cn.hutool.core.codec.Base64.encode(p0, p1);
    }

}
