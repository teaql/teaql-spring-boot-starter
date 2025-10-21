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

public class IndonesianTranslator extends BaseLanguageTranslator{


    // [Location] seharusnya sama dengan atau lebih besar dari [SystemValue], tetapi inputnya adalah [InputValue]
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} seharusnya sama dengan atau lebih besar dari {}, tetapi inputnya adalah {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected Object translateLocation(CheckResult error) {
        return translateLocation(error.getLocation());
    }

    // [Location] seharusnya sama dengan atau lebih kecil dari [SystemValue], tetapi inputnya adalah [InputValue]
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} seharusnya sama dengan atau lebih kecil dari {}, tetapi inputnya adalah {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // Panjang dari [Location] seharusnya sama dengan atau lebih besar dari [SystemValue], tetapi panjang dari [InputValue] adalah [ActualLength]
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "Panjang dari {} seharusnya sama dengan atau lebih besar dari {}, tetapi panjang dari {} adalah {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // Panjang dari [Location] seharusnya sama dengan atau lebih kecil dari [SystemValue], tetapi panjang dari [InputValue] adalah [ActualLength]
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "Panjang dari {} seharusnya sama dengan atau lebih kecil dari {}, tetapi panjang dari {} adalah {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // [Location] seharusnya pada atau setelah [SystemValue], tetapi inputnya adalah [InputValue]
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} seharusnya pada atau setelah {}, tetapi inputnya adalah {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] seharusnya pada atau sebelum [SystemValue], tetapi inputnya adalah [InputValue]
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} seharusnya pada atau sebelum {}, tetapi inputnya adalah {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] diperlukan
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} diperlukan", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Location] dari [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} dari {}", getSimpleLocation(location), getSimpleLocation(parent));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> atribut [Location] dalam [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "atribut {} dalam {}", getSimpleLocation(location), translateLocation(parent));
            }

            // [Location] attribute within the [ArrayLocation] -> atribut [Location] dalam [ArrayLocation]
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "atribut {} dalam {}", getSimpleLocation(location), getArrayLocation(parent));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // [Ordinal] element of the [Parent] -> elemen [Ordinal] dari [Parent]
            return StrUtil.format(
                    "elemen {} dari {}",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }



    /**
     * Indonesian Ordinal: "pertama" for 1st, and "ke-" + number for all others (e.g. ke-2, ke-3).
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        if (sequence == 1) {
            return "pertama";
        }
        // ke- + cardinal number
        return "ke-" + sequence;
    }
}