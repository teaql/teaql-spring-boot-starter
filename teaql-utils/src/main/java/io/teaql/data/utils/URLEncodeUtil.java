package io.teaql.data.utils;

public class URLEncodeUtil {

    public static java.lang.String encode(java.lang.String p0) {
        return cn.hutool.core.net.URLEncodeUtil.encode(p0);
    }

    public static java.lang.String encode(java.lang.String p0, java.nio.charset.Charset p1) {
        return cn.hutool.core.net.URLEncodeUtil.encode(p0, p1);
    }

}
