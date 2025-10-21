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

public class FrenchTranslator extends BaseLanguageTranslator{

    // Le/La [Location] doit être égal ou supérieur à [SystemValue], mais la saisie est [InputValue]
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "Le/La {} doit être égal ou supérieur à {} (ou égale/supérieure, selon le genre), mais la saisie est {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }



    // Le/La [Location] doit être égal ou inférieur à [SystemValue], mais la saisie est [InputValue]
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "Le/La {} doit être égal ou inférieur à {} (ou égale/inférieure, selon le genre), mais la saisie est {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // La longueur de [Location] doit être égale ou supérieure à [SystemValue], mais la longueur de [InputValue] est [ActualLength]
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "La longueur de {} doit être égale ou supérieure à {} caractères, mais la longueur de {} est {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // La longueur de [Location] doit être égale ou inférieure à [SystemValue], mais la longueur de [InputValue] est [ActualLength]
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "La longueur de {} doit être égale ou inférieure à {} caractères, mais la longueur de {} est {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // Le/La [Location] doit être à ou après [SystemValue], mais la saisie est [InputValue]
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "Le/La {} doit être à ou après le {}, mais la saisie est {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // Le/La [Location] doit être à ou avant [SystemValue], mais la saisie est [InputValue]
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "Le/La {} doit être à ou avant le {}, mais la saisie est {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] est requis(e)
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} est requis(e)", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Location] du/de la [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} du/de la {}", getSimpleLocation(location), getSimpleLocation(parent));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> l'attribut [Location] dans le/la [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "l'attribut {} dans le/la {}", getSimpleLocation(location), translateLocation(parent));
            }

            // [Location] attribute within the [ArrayLocation] -> l'attribut [Location] dans [ArrayLocation]
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "l'attribut {} dans {}", getSimpleLocation(location), getArrayLocation(parent));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // [Ordinal] element of the [Parent] -> le/la [Ordinal] élément de [Parent]
            return StrUtil.format(
                    "le/la {} élément de {}",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }


    /**
     * French Ordinal: 1er (premier) for 1st, and number + e for all others (e.g. 2e, 11e).
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        if (sequence == 1) {
            return "1er"; // premier (masculine form)
        }
        return sequence + "e"; // e.g. 2e, 3e, 11e
    }
}