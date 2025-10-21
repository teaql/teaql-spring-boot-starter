package io.teaql.data.language;

import java.util.List;

import cn.hutool.core.text.NamingCase;
import cn.hutool.core.util.StrUtil;

import io.teaql.data.Entity;
import io.teaql.data.NaturalLanguageTranslator;
import io.teaql.data.checker.ArrayLocation;
import io.teaql.data.checker.CheckResult;
import io.teaql.data.checker.HashLocation;
import io.teaql.data.checker.ObjectLocation;

public class JapaneseTranslator extends BaseLanguageTranslator {


    // [Location] は [SystemValue] 以上であるべきですが、しかし、入力は [InputValue] です
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} は {} 以上であるべきですが、しかし、入力は {} です",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected Object translateLocation(CheckResult error) {
        return translateLocation(error.getLocation());
    }

    // [Location] は [SystemValue] 以下であるべきですが、しかし、入力は [InputValue] です
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} は {} 以下であるべきですが、しかし、入力は {} です",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] の長さは [SystemValue] 以上であるべきですが、しかし、[InputValue] の長さは [ActualLength] です
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} の長さは {} 以上であるべきですが、しかし、{} の長さは {} です",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // [Location] の長さは [SystemValue] 以下であるべきですが、しかし、[InputValue] の長さは [ActualLength] です
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} の長さは {} 以下であるべきですが、しかし、{} の長さは {} です",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // [Location] は [SystemValue] 以降であるべきですが、しかし、入力は [InputValue] です
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} は {} 以降であるべきですが、しかし、入力は {} です",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] は [SystemValue] 以前であるべきですが、しかし、入力は [InputValue] です
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} は {} 以前であるべきですが、しかし、入力は {} です",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] は必須です
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} は必須です", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Parent] の [Location]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} の {}", getSimpleLocation(parent), getSimpleLocation(location));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> [Parent] 内の [Location] 属性
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} 内の {} 属性", translateLocation(parent), getSimpleLocation(location));
            }

            // [Location] attribute within the [ArrayLocation] -> [ArrayLocation] 内の [Location] 属性
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "{} 内の {} 属性", getArrayLocation(parent), getSimpleLocation(location));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // [Ordinal] element of the [Parent] -> [Parent] の [Ordinal] 番目の要素
            return StrUtil.format(
                    "{} の {} 番目の要素",
                    translateLocation(location.getParent()),
                    ordinal(((ArrayLocation) location).getIndex()));
        }
        return location.toString();
    }



    /**
     * Japanese Ordinal: Use number + "番目" (banme) as the common and clear form.
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        // number + "番目"
        return sequence + "番目";
    }
}