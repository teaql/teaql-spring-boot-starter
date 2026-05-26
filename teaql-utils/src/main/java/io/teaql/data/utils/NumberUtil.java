package io.teaql.data.utils;

public class NumberUtil {

    public static boolean isGreater(java.math.BigDecimal p0, java.math.BigDecimal p1) {
        return cn.hutool.core.util.NumberUtil.isGreater(p0, p1);
    }

    public static boolean isLess(java.math.BigDecimal p0, java.math.BigDecimal p1) {
        return cn.hutool.core.util.NumberUtil.isLess(p0, p1);
    }

    public static double add(double p0, double p1) {
        return cn.hutool.core.util.NumberUtil.add(p0, p1);
    }

    public static double add(double p0, float p1) {
        return cn.hutool.core.util.NumberUtil.add(p0, p1);
    }

    public static double add(float p0, double p1) {
        return cn.hutool.core.util.NumberUtil.add(p0, p1);
    }

    public static double add(float p0, float p1) {
        return cn.hutool.core.util.NumberUtil.add(p0, p1);
    }

    public static double add(java.lang.Double p0, java.lang.Double p1) {
        return cn.hutool.core.util.NumberUtil.add(p0, p1);
    }

    public static java.lang.Number parseNumber(java.lang.String p0) {
        return cn.hutool.core.util.NumberUtil.parseNumber(p0);
    }

    public static java.math.BigDecimal add(java.lang.Number p0, java.lang.Number p1) {
        return cn.hutool.core.util.NumberUtil.add(p0, p1);
    }

    public static java.math.BigDecimal add(java.lang.Number... p0) {
        return cn.hutool.core.util.NumberUtil.add(p0);
    }

    public static java.math.BigDecimal add(java.lang.String... p0) {
        return cn.hutool.core.util.NumberUtil.add(p0);
    }

    public static java.math.BigDecimal add(java.math.BigDecimal... p0) {
        return cn.hutool.core.util.NumberUtil.add(p0);
    }

    public static java.math.BigDecimal toBigDecimal(java.lang.Number p0) {
        return cn.hutool.core.util.NumberUtil.toBigDecimal(p0);
    }

    public static java.math.BigDecimal toBigDecimal(java.lang.String p0) {
        return cn.hutool.core.util.NumberUtil.toBigDecimal(p0);
    }

}
