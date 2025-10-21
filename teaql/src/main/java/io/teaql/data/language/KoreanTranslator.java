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

public class KoreanTranslator extends BaseLanguageTranslator {


    // [Location] 은/는 [SystemValue] 와 같거나 커야 합니다. 하지만 입력값은 [InputValue] 입니다.
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 은/는 {} 와 같거나 커야 합니다. 하지만 입력값은 {} 입니다.",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }



    // [Location] 은/는 [SystemValue] 와 같거나 작아야 합니다. 하지만 입력값은 [InputValue] 입니다.
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 은/는 {} 와 같거나 작아야 합니다. 하지만 입력값은 {} 입니다.",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] 의 길이는 [SystemValue] 와 같거나 커야 합니다. 하지만 [InputValue] 의 길이는 [ActualLength] 입니다.
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 의 길이는 {} 와 같거나 커야 합니다. 하지만 {} 의 길이는 {} 입니다.",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // [Location] 의 길이는 [SystemValue] 와 같거나 작아야 합니다. 하지만 [InputValue] 의 길이는 [ActualLength] 입니다.
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 의 길이는 {} 와 같거나 작아야 합니다. 하지만 {} 의 길이는 {} 입니다.",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // [Location] 은/는 [SystemValue] 또는 그 이후여야 합니다. 하지만 입력값은 [InputValue] 입니다.
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 은/는 {} 또는 그 이후여야 합니다. 하지만 입력값은 {} 입니다.",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] 은/는 [SystemValue] 또는 그 이전이어야 합니다. 하지만 입력값은 [InputValue] 입니다.
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 은/는 {} 또는 그 이전이어야 합니다. 하지만 입력값은 {} 입니다.",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] 은/는 필수 항목입니다
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} 은/는 필수 항목입니다", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Parent] 의 [Location]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} 의 {}", getSimpleLocation(parent), getSimpleLocation(location));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> [Parent] 내의 [Location] 속성
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} 내의 {} 속성", translateLocation(parent), getSimpleLocation(location));
            }

            // [Location] attribute within the [ArrayLocation] -> [ArrayLocation] 내의 [Location] 속성
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "{} 내의 {} 속성", getArrayLocation(parent), getSimpleLocation(location));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // [Ordinal] element of the [Parent] -> [Parent] 의 [Ordinal] 번째 요소
            return StrUtil.format(
                    "{} 의 {} 번째 요소",
                    translateLocation(location.getParent()),
                    ordinal(((ArrayLocation) location).getIndex()));
        }
        return location.toString();
    }



    /**
     * Korean Ordinal: Use number + "번째" (beonjjae) as the common and clear form.
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        // number + "번째"
        return sequence + "번째";
    }
}