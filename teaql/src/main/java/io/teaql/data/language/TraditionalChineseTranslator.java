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

public class TraditionalChineseTranslator extends BaseLanguageTranslator {

    // [Location] 應該等於或大於 [SystemValue]，但輸入為 [InputValue]
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 應該等於或大於 {}，但輸入為 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected Object translateLocation(CheckResult error) {
        return translateLocation(error.getLocation());
    }

    // [Location] 應該等於或小於 [SystemValue]，但輸入為 [InputValue]
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 應該等於或小於 {}，但輸入為 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] 的長度應該等於或大於 [SystemValue]，但 [InputValue] 的長度是 [ActualLength]
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 的長度應該等於或大於 {}，但 {} 的長度是 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // [Location] 的長度應該等於或小於 [SystemValue]，但 [InputValue] 的長度是 [ActualLength]
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 的長度應該等於或小於 {}，但 {} 的長度是 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // [Location] 應該在 [SystemValue] 或之後，但輸入為 [InputValue]
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 應該在 {} 或之後，但輸入為 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] 應該在 [SystemValue] 或之前，但輸入為 [InputValue]
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 應該在 {} 或之前，但輸入為 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] 是必填的
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} 是必填的", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Parent] 的 [Location]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} 的 {}", getSimpleLocation(parent), getSimpleLocation(location));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> [Parent] 內的 [Location] 屬性
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} 內的 {} 屬性", translateLocation(parent), getSimpleLocation(location));
            }

            // [Location] attribute within the [ArrayLocation] -> [ArrayLocation] 內的 [Location] 屬性
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "{} 內的 {} 屬性", getArrayLocation(parent), getSimpleLocation(location));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // [Ordinal] element of the [Parent] -> [Parent] 的第 [Ordinal] 個元素
            return StrUtil.format(
                    "{}的{}元素",
                    translateLocation(location.getParent()),
                    ordinal(((ArrayLocation) location).getIndex()));
        }
        return location.toString();
    }



    /**
     * Traditional Chinese Ordinal: Use "第" (dì) + number + "個" (gè).
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        // "第" + number + "個"
        return "第" + sequence + "個";
    }
}
