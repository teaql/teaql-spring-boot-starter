package io.teaql.data.utils;

public class DateUtil {

    public static java.time.LocalDateTime toLocalDateTime(java.util.Calendar p0) {
        return cn.hutool.core.date.DateUtil.toLocalDateTime(p0);
    }

    public static java.time.LocalDateTime toLocalDateTime(java.time.Instant p0) {
        return cn.hutool.core.date.DateUtil.toLocalDateTime(p0);
    }

    public static java.time.LocalDateTime toLocalDateTime(java.util.Date p0) {
        return cn.hutool.core.date.DateUtil.toLocalDateTime(p0);
    }

}
