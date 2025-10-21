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

public class ArabicTranslator extends BaseLanguageTranslator{


    // يجب أن يكون [Location] مساويًا أو أكبر من [SystemValue]، لكن المُدخل هو [InputValue]
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "يجب أن يكون {} مساويًا أو أكبر من {}، لكن المُدخل هو {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected Object translateLocation(CheckResult error) {
        return translateLocation(error.getLocation());
    }

    // يجب أن يكون [Location] مساويًا أو أصغر من [SystemValue]، لكن المُدخل هو [InputValue]
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "يجب أن يكون {} مساويًا أو أصغر من {}، لكن المُدخل هو {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // يجب أن يكون طول [Location] مساويًا أو أكبر من [SystemValue]، لكن طول [InputValue] هو [ActualLength]
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "يجب أن يكون طول {} مساويًا أو أكبر من {}، لكن طول {} هو {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // يجب أن يكون طول [Location] مساويًا أو أصغر من [SystemValue]، لكن طول [InputValue] هو [ActualLength]
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "يجب أن يكون طول {} مساويًا أو أصغر من {}، لكن طول {} هو {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // يجب أن يكون [Location] في أو بعد [SystemValue]، لكن المُدخل هو [InputValue]
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "يجب أن يكون {} في أو بعد {}، لكن المُدخل هو {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // يجب أن يكون [Location] في أو قبل [SystemValue]، لكن المُدخل هو [InputValue]
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "يجب أن يكون {} في أو قبل {}، لكن المُدخل هو {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] مطلوب
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} مطلوب", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Location] لـ [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} لـ {}", getSimpleLocation(location), getSimpleLocation(parent));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> سمة [Location] داخل [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "سمة {} داخل {}", getSimpleLocation(location), translateLocation(parent));
            }

            // [Location] attribute within the [ArrayLocation] -> سمة [Location] داخل [ArrayLocation]
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "سمة {} داخل {}", getSimpleLocation(location), getArrayLocation(parent));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // [Ordinal] element of the [Parent] -> العنصر الـ [Ordinal] من [Parent]
            return StrUtil.format(
                    "العنصر الـ {} من {}",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }



    /**
     * Arabic Ordinal: Use number + dot (.) as the standard general abbreviation.
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        // Standard abbreviation for ordinal numbers in Arabic is number followed by a dot.
        return sequence + ".";
    }
}