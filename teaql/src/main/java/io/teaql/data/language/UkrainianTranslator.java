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

public class UkrainianTranslator extends BaseLanguageTranslator {


    // [Location] повинен бути рівним або більшим за [SystemValue], але ввідне значення [InputValue]
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} повинен бути рівним або більшим за {}, але ввідне значення {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }


    // [Location] повинен бути рівним або меншим за [SystemValue], але ввідне значення [InputValue]
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} повинен бути рівним або меншим за {}, але ввідне значення {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // Довжина [Location] повинна бути рівною або більшою за [SystemValue], але довжина [InputValue] становить [ActualLength]
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "Довжина {} повинна бути рівною або більшою за {}, але довжина {} становить {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // Довжина [Location] повинна бути рівною або меншою за [SystemValue], але довжина [InputValue] становить [ActualLength]
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "Довжина {} повинна бути рівною або меншою за {}, але довжина {} становить {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // [Location] повинен бути у або після [SystemValue], але ввідне значення [InputValue]
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} повинен бути у або після {}, але ввідне значення {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] повинен бути у або до [SystemValue], але ввідне значення [InputValue]
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} повинен бути у або до {}, але ввідне значення {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] є обов'язковим
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} є обов'язковим", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Location] з [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} з {}", getSimpleLocation(location), getSimpleLocation(parent));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> атрибут [Location] всередині [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "атрибут {} всередині {}", getSimpleLocation(location), translateLocation(parent));
            }

            // [Location] attribute within the [ArrayLocation] -> атрибут [Location] всередині [ArrayLocation]
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "атрибут {} всередині {}", getSimpleLocation(location), getArrayLocation(parent));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // [Ordinal] element of the [Parent] -> [Ordinal] елемент з [Parent]
            return StrUtil.format(
                    "{} елемент з {}",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }



    /**
     * Ukrainian Ordinal: Use number + dot (.) as the standard general abbreviation.
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        // Standard abbreviation for ordinal numbers in Ukrainian is number followed by a dot.
        return sequence + ".";
    }
}

