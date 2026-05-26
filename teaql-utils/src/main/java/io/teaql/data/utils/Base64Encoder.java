package io.teaql.data.utils;

public class Base64Encoder {

    public static byte[] encodeUrlSafe(byte[] p0, boolean p1) {
        return cn.hutool.core.codec.Base64Encoder.encodeUrlSafe(p0, p1);
    }

    public static java.lang.String encodeUrlSafe(byte[] p0) {
        return cn.hutool.core.codec.Base64Encoder.encodeUrlSafe(p0);
    }

    public static java.lang.String encodeUrlSafe(java.lang.CharSequence p0) {
        return cn.hutool.core.codec.Base64Encoder.encodeUrlSafe(p0);
    }

    public static java.lang.String encodeUrlSafe(java.lang.CharSequence p0, java.nio.charset.Charset p1) {
        return cn.hutool.core.codec.Base64Encoder.encodeUrlSafe(p0, p1);
    }

}
