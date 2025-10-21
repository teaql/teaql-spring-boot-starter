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

public class PortugueseTranslator extends BaseLanguageTranslator {


    // O/A [Location] deve ser igual ou maior que [SystemValue], mas a entrada é [InputValue]
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "O/A {} deve ser igual ou maior que {}, mas a entrada é {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected Object translateLocation(CheckResult error) {
        return translateLocation(error.getLocation());
    }

    // O/A [Location] deve ser igual ou menor que [SystemValue], mas a entrada é [InputValue]
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "O/A {} deve ser igual ou menor que {}, mas a entrada é {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // O comprimento de [Location] deve ser igual ou maior que [SystemValue], mas o comprimento de [InputValue] é [ActualLength]
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "O comprimento de {} deve ser igual ou maior que {}, mas o comprimento de {} é {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // O comprimento de [Location] deve ser igual ou menor que [SystemValue], mas o comprimento de [InputValue] é [ActualLength]
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "O comprimento de {} deve ser igual ou menor que {}, mas o comprimento de {} é {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // O/A [Location] deve ser em ou após [SystemValue], mas a entrada é [InputValue]
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "O/A {} deve ser em ou após {}, mas a entrada é {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // O/A [Location] deve ser em ou antes [SystemValue], mas a entrada é [InputValue]
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "O/A {} deve ser em ou antes {}, mas a entrada é {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] é obrigatório(a)
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} é obrigatório(a)", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Location] de o/a [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} de o/a {}", getSimpleLocation(location), getSimpleLocation(parent));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> atributo [Location] dentro de o/a [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "atributo {} dentro de o/a {}", getSimpleLocation(location), translateLocation(parent));
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
            // [Ordinal] element of the [Parent] -> o/a [Ordinal] elemento de [Parent]
            return StrUtil.format(
                    "o/a {} elemento de {}",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }



    /**
     * Portuguese Ordinal: Use number + "º" (masculine) as the standard general abbreviation.
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        // Standard abbreviation for ordinal numbers in Portuguese (masculine form)
        return sequence + "º";
    }
}