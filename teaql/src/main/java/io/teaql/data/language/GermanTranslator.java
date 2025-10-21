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

public class GermanTranslator extends BaseLanguageTranslator{

    // Der/Die/Das [Location] sollte gleich oder größer als [SystemValue] sein, aber die Eingabe ist [InputValue]
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "Der/Die/Das {} sollte gleich oder größer als {} sein, aber die Eingabe ist {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected Object translateLocation(CheckResult error) {
        return translateLocation(error.getLocation());
    }

    // Der/Die/Das [Location] sollte gleich oder kleiner als [SystemValue] sein, aber die Eingabe ist [InputValue]
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "Der/Die/Das {} sollte gleich oder kleiner als {} sein, aber die Eingabe ist {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // Die Länge von [Location] sollte gleich oder größer als [SystemValue] sein, aber die Länge von [InputValue] ist [ActualLength]
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "Die Länge von {} sollte gleich oder größer als {} sein, aber die Länge von {} ist {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // Die Länge von [Location] sollte gleich oder kleiner als [SystemValue] sein, aber die Länge von [InputValue] ist [ActualLength]
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "Die Länge von {} sollte gleich oder kleiner als {} sein, aber die Länge von {} ist {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // Der/Die/Das [Location] sollte am oder nach dem [SystemValue] liegen, aber die Eingabe ist [InputValue]
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "Der/Die/Das {} sollte am oder nach dem {} liegen, aber die Eingabe ist {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // Der/Die/Das [Location] sollte am oder vor dem [SystemValue] liegen, aber die Eingabe ist [InputValue]
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "Der/Die/Das {} sollte am oder vor dem {} liegen, aber die Eingabe ist {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] ist erforderlich
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} ist erforderlich", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Location] des/der [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} des/der {}", getSimpleLocation(location), getSimpleLocation(parent));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> Attribut [Location] innerhalb des/der [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "Attribut {} innerhalb des/der {}", getSimpleLocation(location), translateLocation(parent));
            }

            // [Location] attribute within the [ArrayLocation] -> Attribut [Location] innerhalb der [ArrayLocation]
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "Attribut {} innerhalb der {}", getSimpleLocation(location), getArrayLocation(parent));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // [Ordinal] element of the [Parent] -> das [Ordinal] Element von [Parent]
            return StrUtil.format(
                    "das {} Element von {}",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }



    /**
     * German Ordinal: Use number + dot (.) as the standard general abbreviation.
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        // Standard abbreviation for ordinal numbers in German is number followed by a dot.
        return sequence + ".";
    }
}