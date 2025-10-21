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

public class SpanishTranslator extends BaseLanguageTranslator {


    // El/La [Location] debe ser igual o mayor que [SystemValue], pero el valor ingresado es [InputValue]
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "El/La {} debe ser igual o mayor que {}, pero el valor ingresado es {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected Object translateLocation(CheckResult error) {
        return translateLocation(error.getLocation());
    }

    // El/La [Location] debe ser igual o menor que [SystemValue], pero el valor ingresado es [InputValue]
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "El/La {} debe ser igual o menor que {}, pero el valor ingresado es {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // La longitud de [Location] debe ser igual o mayor que [SystemValue], pero la longitud de [InputValue] es [ActualLength]
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "La longitud de {} debe ser igual o mayor que {}, pero la longitud de {} es {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // La longitud de [Location] debe ser igual o menor que [SystemValue], pero la longitud de [InputValue] es [ActualLength]
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "La longitud de {} debe ser igual o menor que {}, pero la longitud de {} es {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // El/La [Location] debe ser en o después de [SystemValue], pero el valor ingresado es [InputValue]
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "El/La {} debe ser en o después de {}, pero el valor ingresado es {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // El/La [Location] debe ser en o antes de [SystemValue], pero el valor ingresado es [InputValue]
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "El/La {} debe ser en o antes de {}, pero el valor ingresado es {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] es requerido/a
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} es requerido/a", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Location] de el/la [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} de el/la {}", getSimpleLocation(location), getSimpleLocation(parent));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> atributo [Location] dentro de el/la [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "atributo {} dentro de el/la {}", getSimpleLocation(location), translateLocation(parent));
            }

            // [Location] attribute within the [ArrayLocation] -> atributo [Location] dentro de [ArrayLocation]
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "atributo {} dentro de {}", getSimpleLocation(location), getArrayLocation(parent));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // [Ordinal] element of the [Parent] -> el/la [Ordinal] elemento de [Parent]
            return StrUtil.format(
                    "el/la {} elemento de {}",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }



    /**
     * Spanish Ordinal: Use number + "º" (masculine) as the standard general abbreviation.
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        // Standard abbreviation for ordinal numbers in Spanish (masculine form)
        return sequence + "º";
    }
}