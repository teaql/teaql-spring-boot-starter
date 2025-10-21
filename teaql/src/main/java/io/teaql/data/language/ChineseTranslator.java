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

public class ChineseTranslator extends BaseLanguageTranslator{


    protected void translateMin(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 应该大于等于 {}，但输入值为 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }



    protected void translateMax(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 应该小于等于 {}，但输入值为 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected void translateMinStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 的长度应大于等于 {}，但实际长度为 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    protected void translateMaxStrLen(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 的长度应小于等于 {}，但实际长度为 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        StrUtil.length((CharSequence) error.getInputValue()));
        error.setNaturalLanguageStatement(message);
    }

    protected void translateMinDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 应该在 {} 或之后，但输入值为 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected void translateMaxDate(CheckResult error) {
        String message =
                StrUtil.format(
                        "{} 应该在 {} 或之前，但输入值为 {}",
                        translateLocation(error),
                        error.getSystemValue(),
                        error.getInputValue());
        error.setNaturalLanguageStatement(message);
    }

    protected void translateRequired(CheckResult error) {
        String message = StrUtil.format("{} 是必填项", translateLocation(error));
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
                        "{} 的 {}", getSimpleLocation(location), getSimpleLocation(parent));
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
                        "{} 属性在 {}", getSimpleLocation(location), translateLocation(parent));
            }

            // product
            // skuList[0].name
            if (parent instanceof ArrayLocation) {
                return StrUtil.format(
                        "{} 属性在 {}", getSimpleLocation(location), getArrayLocation(parent));
            }
        }

        return location.toString();
    }

    protected Object getArrayLocation(ObjectLocation location) {
        if (location instanceof ArrayLocation) {
            return StrUtil.format(
                    "{} 的 {} 元素",
                    ordinal(((ArrayLocation) location).getIndex()),
                    translateLocation(location.getParent()));
        }
        return location.toString();
    }


    public String ordinal(int index) {
        int sequence = index + 1;
        return "第" + sequence +"个";
    }
}

