package io.teaql.data.utils;

public class StrUtil {

    public static boolean contains(java.lang.CharSequence p0, char p1) {
        return cn.hutool.core.util.StrUtil.contains(p0, p1);
    }

    public static boolean contains(java.lang.CharSequence p0, java.lang.CharSequence p1) {
        return cn.hutool.core.util.StrUtil.contains(p0, p1);
    }

    public static boolean endWith(java.lang.CharSequence p0, char p1) {
        return cn.hutool.core.util.StrUtil.endWith(p0, p1);
    }

    public static boolean endWith(java.lang.CharSequence p0, java.lang.CharSequence p1) {
        return cn.hutool.core.util.StrUtil.endWith(p0, p1);
    }

    public static boolean endWith(java.lang.CharSequence p0, java.lang.CharSequence p1, boolean p2) {
        return cn.hutool.core.util.StrUtil.endWith(p0, p1, p2);
    }

    public static boolean endWith(java.lang.CharSequence p0, java.lang.CharSequence p1, boolean p2, boolean p3) {
        return cn.hutool.core.util.StrUtil.endWith(p0, p1, p2, p3);
    }

    public static boolean isEmpty(java.lang.CharSequence p0) {
        return cn.hutool.core.util.StrUtil.isEmpty(p0);
    }

    public static boolean isNotEmpty(java.lang.CharSequence p0) {
        return cn.hutool.core.util.StrUtil.isNotEmpty(p0);
    }

    public static boolean startWith(java.lang.CharSequence p0, char p1) {
        return cn.hutool.core.util.StrUtil.startWith(p0, p1);
    }

    public static boolean startWith(java.lang.CharSequence p0, java.lang.CharSequence p1) {
        return cn.hutool.core.util.StrUtil.startWith(p0, p1);
    }

    public static boolean startWith(java.lang.CharSequence p0, java.lang.CharSequence p1, boolean p2) {
        return cn.hutool.core.util.StrUtil.startWith(p0, p1, p2);
    }

    public static boolean startWith(java.lang.CharSequence p0, java.lang.CharSequence p1, boolean p2, boolean p3) {
        return cn.hutool.core.util.StrUtil.startWith(p0, p1, p2, p3);
    }

    public static boolean startWithIgnoreCase(java.lang.CharSequence p0, java.lang.CharSequence p1) {
        return cn.hutool.core.util.StrUtil.startWithIgnoreCase(p0, p1);
    }

    public static io.teaql.data.utils.StrBuilder strBuilder(java.lang.CharSequence... p0) {
        return new io.teaql.data.utils.StrBuilder(cn.hutool.core.util.StrUtil.strBuilder(p0));
    }

    public static io.teaql.data.utils.StrBuilder strBuilder() {
        return new io.teaql.data.utils.StrBuilder(cn.hutool.core.util.StrUtil.strBuilder());
    }

    public static io.teaql.data.utils.StrBuilder strBuilder(int p0) {
        return new io.teaql.data.utils.StrBuilder(cn.hutool.core.util.StrUtil.strBuilder(p0));
    }

    public static int length(java.lang.CharSequence p0) {
        return cn.hutool.core.util.StrUtil.length(p0);
    }

    public static java.lang.String format(java.lang.CharSequence p0, java.lang.Object... p1) {
        return cn.hutool.core.util.StrUtil.format(p0, p1);
    }

    public static <T> java.lang.String join(java.lang.CharSequence p0, java.lang.Iterable<T> p1) {
        return cn.hutool.core.util.StrUtil.join(p0, p1);
    }

    public static java.lang.String join(java.lang.CharSequence p0, java.lang.Object... p1) {
        return cn.hutool.core.util.StrUtil.join(p0, p1);
    }

    public static java.lang.String removePrefix(java.lang.CharSequence p0, java.lang.CharSequence p1) {
        return cn.hutool.core.util.StrUtil.removePrefix(p0, p1);
    }

    public static java.lang.String removeSuffix(java.lang.CharSequence p0, java.lang.CharSequence p1) {
        return cn.hutool.core.util.StrUtil.removeSuffix(p0, p1);
    }

    public static java.lang.String repeatAndJoin(java.lang.CharSequence p0, int p1, java.lang.CharSequence p2) {
        return cn.hutool.core.util.StrUtil.repeatAndJoin(p0, p1, p2);
    }

    public static java.lang.String sub(java.lang.CharSequence p0, int p1, int p2) {
        return cn.hutool.core.util.StrUtil.sub(p0, p1, p2);
    }

    public static java.lang.String subSuf(java.lang.CharSequence p0, int p1) {
        return cn.hutool.core.util.StrUtil.subSuf(p0, p1);
    }

    public static java.lang.String unWrap(java.lang.CharSequence p0, char p1) {
        return cn.hutool.core.util.StrUtil.unWrap(p0, p1);
    }

    public static java.lang.String unWrap(java.lang.CharSequence p0, char p1, char p2) {
        return cn.hutool.core.util.StrUtil.unWrap(p0, p1, p2);
    }

    public static java.lang.String unWrap(java.lang.CharSequence p0, java.lang.String p1, java.lang.String p2) {
        return cn.hutool.core.util.StrUtil.unWrap(p0, p1, p2);
    }

    public static java.lang.String upperFirst(java.lang.CharSequence p0) {
        return cn.hutool.core.util.StrUtil.upperFirst(p0);
    }

    public static java.lang.String upperFirstAndAddPre(java.lang.CharSequence p0, java.lang.String p1) {
        return cn.hutool.core.util.StrUtil.upperFirstAndAddPre(p0, p1);
    }

    public static java.lang.String wrap(java.lang.CharSequence p0, java.lang.CharSequence p1) {
        return cn.hutool.core.util.StrUtil.wrap(p0, p1);
    }

    public static java.lang.String wrap(java.lang.CharSequence p0, java.lang.CharSequence p1, java.lang.CharSequence p2) {
        return cn.hutool.core.util.StrUtil.wrap(p0, p1, p2);
    }

    public static java.lang.String wrapIfMissing(java.lang.CharSequence p0, java.lang.CharSequence p1, java.lang.CharSequence p2) {
        return cn.hutool.core.util.StrUtil.wrapIfMissing(p0, p1, p2);
    }

    public static java.lang.String format(java.lang.CharSequence p0, java.util.Map<?, ?> p1) {
        return cn.hutool.core.util.StrUtil.format(p0, p1);
    }

    public static java.lang.String format(java.lang.CharSequence p0, java.util.Map<?, ?> p1, boolean p2) {
        return cn.hutool.core.util.StrUtil.format(p0, p1, p2);
    }

    public static java.lang.String[] split(java.lang.CharSequence p0, int p1) {
        return cn.hutool.core.util.StrUtil.split(p0, p1);
    }

    public static java.util.List<java.lang.String> split(java.lang.CharSequence p0, char p1) {
        return cn.hutool.core.util.StrUtil.split(p0, p1);
    }

    public static java.util.List<java.lang.String> split(java.lang.CharSequence p0, char p1, boolean p2, boolean p3) {
        return cn.hutool.core.util.StrUtil.split(p0, p1, p2, p3);
    }

    public static java.util.List<java.lang.String> split(java.lang.CharSequence p0, char p1, int p2) {
        return cn.hutool.core.util.StrUtil.split(p0, p1, p2);
    }

    public static java.util.List<java.lang.String> split(java.lang.CharSequence p0, char p1, int p2, boolean p3, boolean p4) {
        return cn.hutool.core.util.StrUtil.split(p0, p1, p2, p3, p4);
    }

    public static <R> java.util.List<R> split(java.lang.CharSequence p0, char p1, int p2, boolean p3, java.util.function.Function<java.lang.String, R> p4) {
        return cn.hutool.core.util.StrUtil.split(p0, p1, p2, p3, p4);
    }

    public static java.util.List<java.lang.String> split(java.lang.CharSequence p0, java.lang.CharSequence p1) {
        return cn.hutool.core.util.StrUtil.split(p0, p1);
    }

    public static java.util.List<java.lang.String> split(java.lang.CharSequence p0, java.lang.CharSequence p1, boolean p2, boolean p3) {
        return cn.hutool.core.util.StrUtil.split(p0, p1, p2, p3);
    }

    public static java.util.List<java.lang.String> split(java.lang.CharSequence p0, java.lang.CharSequence p1, int p2, boolean p3, boolean p4) {
        return cn.hutool.core.util.StrUtil.split(p0, p1, p2, p3, p4);
    }

}
