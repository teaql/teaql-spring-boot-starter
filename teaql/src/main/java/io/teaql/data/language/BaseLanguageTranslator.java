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

public class BaseLanguageTranslator implements NaturalLanguageTranslator {

    @Override
    public List<CheckResult> translateError(Entity pEntity, List<CheckResult> errors) {
        for (CheckResult error : errors) {
            translate(error);
        }
        return errors;
    }
    protected void translate(CheckResult error) {
        switch (error.getRuleId()) {
            case MIN:
                translateMin(error);
                break;
            case MAX:
                translateMax(error);
                break;
            case MIN_STR_LEN:
                translateMinStrLen(error);
                break;
            case MAX_STR_LEN:
                translateMaxStrLen(error);
                break;
            case MIN_DATE:
                translateMinDate(error);
                break;
            case MAX_DATE:
                translateMaxDate(error);
                break;
            case REQUIRED:
                translateRequired(error);
                break;
        }
    }

    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "The {} should be equal or greater than {}, but input is {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected Object translateLocation(CheckResult error) {
        return translateLocation(error.getLocation());
    }

    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "The {} should be equal or less than {}, but input is {} ",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "The length of {} should be equal or greater than {}, but the length of {} is {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "The length of {} should be equal or less than {}, but the length of {} is {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "The {} should be at or after {}, but input is {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "The {} should be at or before {}, but input is {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("The {} is required", translateLocation(error));
        error.setNaturalLanguageStatement(message);
    }

    protected String translateLocation(ObjectLocation location) {
        // sku

        // product
        // name
        // quantity
        if (location.isFirstLevel()) {
            return getSimpleLocation(location);
        }

        if (location.isSecondLevel()) {
            ObjectLocation parent = location.getParent();
            // sku
            // product.name
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} of the {}", getSimpleLocation(location), getSimpleLocation(parent));
            }

            // product
            // skuList[0]
            if (parent instanceof ArrayLocation) {
            }
        }

        if (location.isThirdLevel()) {
            // sku
            // product.category.name
            ObjectLocation parent = location.getParent();
            if (parent instanceof HashLocation) {
                return StrUtil.format(
                        "{} attribute within the {}", getSimpleLocation(location), translateLocation(parent));
            }

            // product
            // skuList[0].name
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "{} attribute within the {}", getSimpleLocation(location), getArrayLocation(parent));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            return StrUtil.format(
                    "{} element of the {}",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }

    protected String getSimpleLocation(ObjectLocation location) {
        if (location instanceof HashLocation) {
            return convertToTitleCase(((HashLocation) location).getMember());
        }
        return location.toString();
    }

    public static String convertToTitleCase(String input) {
        StringBuilder result = new StringBuilder();

        for (int i = 0; i < input.length(); i++) {
            char currentChar = input.charAt(i);
            if (i == 0 || Character.isUpperCase(currentChar)) {
                if (i != 0) {
                    result.append(" ");  // 插入空格分隔单词
                }
                result.append(Character.toUpperCase(currentChar));  // 将首字母大写
            } else {
                result.append(Character.toLowerCase(currentChar));  // 其他字母小写
            }
        }

        return result.toString();
    }

    public String ordinal(int index) {
        int sequence = index + 1;
        String[] suffixes = new String[] {"th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th"};
        switch (sequence % 100) {
            case 11:
            case 12:
            case 13:
                return sequence + "th";
            default:
                return sequence + suffixes[sequence % 10];
        }
    }
}
