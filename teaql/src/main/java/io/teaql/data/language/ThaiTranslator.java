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

public class ThaiTranslator extends BaseLanguageTranslator {


    // [Location] ควรจะเท่ากับหรือมากกว่า [SystemValue] แต่ข้อมูลที่ป้อนคือ [InputValue]
    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} ควรจะเท่ากับหรือมากกว่า {} แต่ข้อมูลที่ป้อนคือ {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected Object translateLocation(CheckResult error) {
        return translateLocation(error.getLocation());
    }

    // [Location] ควรจะเท่ากับหรือน้อยกว่า [SystemValue] แต่ข้อมูลที่ป้อนคือ [InputValue]
    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} ควรจะเท่ากับหรือน้อยกว่า {} แต่ข้อมูลที่ป้อนคือ {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // ความยาวของ [Location] ควรจะเท่ากับหรือมากกว่า [SystemValue] แต่ความยาวของ [InputValue] คือ [ActualLength]
    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "ความยาวของ {} ควรจะเท่ากับหรือมากกว่า {} แต่ความยาวของ {} คือ {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // ความยาวของ [Location] ควรจะเท่ากับหรือน้อยกว่า [SystemValue] แต่ความยาวของ [InputValue] คือ [ActualLength]
    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "ความยาวของ {} ควรจะเท่ากับหรือน้อยกว่า {} แต่ความยาวของ {} คือ {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    // [Location] ควรจะตรงกับหรือหลัง [SystemValue] แต่ข้อมูลที่ป้อนคือ [InputValue]
    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} ควรจะตรงกับหรือหลัง {} แต่ข้อมูลที่ป้อนคือ {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] ควรจะตรงกับหรือก่อน [SystemValue] แต่ข้อมูลที่ป้อนคือ [InputValue]
    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} ควรจะตรงกับหรือก่อน {} แต่ข้อมูลที่ป้อนคือ {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    // [Location] เป็นสิ่งจำเป็น
    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} เป็นสิ่งจำเป็น", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] of the [Parent] -> [Location] ของ [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} ของ {}", getSimpleLocation(location), getSimpleLocation(parent));
            }

            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            ObjectLocation parent = location.getParent();
            // [Location] attribute within the [Parent] -> คุณสมบัติ [Location] ภายใน [Parent]
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "คุณสมบัติ {} ภายใน {}", getSimpleLocation(location), translateLocation(parent));
            }

            // [Location] attribute within the [ArrayLocation] -> คุณสมบัติ [Location] ภายใน [ArrayLocation]
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "คุณสมบัติ {} ภายใน {}", getSimpleLocation(location), getArrayLocation(parent));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            // [Ordinal] element of the [Parent] -> องค์ประกอบที่ [Ordinal] ของ [Parent]
            return StrUtil.format(
                    "องค์ประกอบที่ {} ของ {}",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }



    /**
     * Thai Ordinal: Use "ที่" (thîi) + number.
     */
    public String ordinal(int index) {
        int sequence = index + 1;
        // "ที่" + number
        return "ที่" + sequence;
    }
}