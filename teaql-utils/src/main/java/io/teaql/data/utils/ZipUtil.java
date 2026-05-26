package io.teaql.data.utils;

public class ZipUtil {

    public static byte[] gzip(byte[] p0) {
        return cn.hutool.core.util.ZipUtil.gzip(p0);
    }

    public static byte[] gzip(java.io.File p0) {
        return cn.hutool.core.util.ZipUtil.gzip(p0);
    }

    public static byte[] gzip(java.io.InputStream p0) {
        return cn.hutool.core.util.ZipUtil.gzip(p0);
    }

    public static byte[] gzip(java.io.InputStream p0, int p1) {
        return cn.hutool.core.util.ZipUtil.gzip(p0, p1);
    }

    public static byte[] gzip(java.lang.String p0, java.lang.String p1) {
        return cn.hutool.core.util.ZipUtil.gzip(p0, p1);
    }

    public static byte[] unGzip(byte[] p0) {
        return cn.hutool.core.util.ZipUtil.unGzip(p0);
    }

    public static byte[] unGzip(java.io.InputStream p0) {
        return cn.hutool.core.util.ZipUtil.unGzip(p0);
    }

    public static byte[] unGzip(java.io.InputStream p0, int p1) {
        return cn.hutool.core.util.ZipUtil.unGzip(p0, p1);
    }

    public static java.lang.String unGzip(byte[] p0, java.lang.String p1) {
        return cn.hutool.core.util.ZipUtil.unGzip(p0, p1);
    }

}
