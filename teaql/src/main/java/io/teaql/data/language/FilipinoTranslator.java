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

public class FilipinoTranslator extends BaseLanguageTranslator{



    // "The [Location] should be equal or greater than [SystemValue], but input is [InputValue]"
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "Ang {} ay dapat katumbas o mas malaki kaysa {}, ngunit ang input ay {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected Object translateLocation(CheckResult error) {
        return translateLocation(error.getLocation());
    }

    // "The [Location] should be equal or less than [SystemValue], but input is [InputValue]"
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "Ang {} ay dapat katumbas o mas maliit kaysa {}, ngunit ang input ay {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // "The length of [Location] should be equal or greater than [SystemValue], but the length of [InputValue] is [ActualLength]"
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "Ang haba ng {} ay dapat katumbas o mas malaki kaysa {}, ngunit ang haba ng {} ay {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // "The length of [Location] should be equal or less than [SystemValue], but the length of [InputValue] is [ActualLength]"
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "Ang haba ng {} ay dapat katumbas o mas maliit kaysa {}, ngunit ang haba ng {} ay {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // "The [Location] should be at or after [SystemValue], but input is [InputValue]"
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "Ang {} ay dapat kasabay o pagkatapos ng {}, ngunit ang input ay {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // "The [Location] should be at or before [SystemValue], but input is [InputValue]"
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "Ang {} ay dapat kasabay o bago ang {}, ngunit ang input ay {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // "The [Location] is required"
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("Ang {} ay kinakailangan", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // product.name -> ang pangalan ng produkto
            if (parent instanceof HashLocation) {
                // "[Location] of the [Parent]" -> "ang [Location] ng [Parent]"
                return StrUtil.format(
                        "ang {} ng {}", getSimpleLocation(location), getSimpleLocation(parent));
            }

            // skuList[0]
            if (parent instanceof ArrayLocation) {
                // This scenario doesn't typically apply at the second level unless the root is an array.
            }
        }

        if (location.isThirdLevel()) {
            // product.category.name
            ObjectLocation parent = location.getParent();
            if (parent instanceof HashLocation) {
                // "[Location] attribute within the [Parent]" -> "ang katangian ng [Location] sa loob ng [Parent]"
                return StrUtil.format(
                        "ang katangian ng {} sa loob ng {}", getSimpleLocation(location), translateLocation(parent));
            }

            // skuList[0].name
            if (parent instanceof ArrayLocation) {
                // "[Location] attribute within the [ArrayLocation]"
                return StrUtil.format(
                        "ang katangian ng {} sa {}", getSimpleLocation(location), getArrayLocation(parent));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // "[Ordinal] element of the [Parent]" -> "ang ika-[Ordinal] na elemento ng [Parent]"
            return StrUtil.format(
                    "ika-{} elemento ng {}",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }



    /**
     * Custom implementation for Tagalog/Filipino ordinal numbers.
     * Rule: Cardinal number is prefixed with 'ika-'.
     * Example: 1 (isa) -> ika-1 (ika-isa); 2 (dalawa) -> ika-2 (ika-dalawa)
     * For simplicity, we use the numerical form (e.g., "ika-1", "ika-2").
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        // Tagalog ordinal form: ika- + number
        return "ika-" + sequence;
    }
}
