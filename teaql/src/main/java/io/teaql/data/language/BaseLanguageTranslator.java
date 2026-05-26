package io.teaql.data.language;

import java.util.List;

import io.teaql.data.utils.NamingCase;
import io.teaql.data.utils.StrUtil;

import io.teaql.data.Entity;
import io.teaql.data.NaturalLanguageTranslator;
import io.teaql.data.checker.ArrayLocation;
import io.teaql.data.checker.CheckResult;
import io.teaql.data.checker.HashLocation;
import io.teaql.data.checker.ObjectLocation;

public class BaseLanguageTranslator implements NaturalLanguageTranslator {

    private static cn.hutool.json.JSONObject i18nDict;
    private static boolean loaded = false;

    private static synchronized void loadDict() {
        if (loaded) {
            return;
        }
        try {
            String path = System.getProperty("teaql.i18n.path");
            String jsonStr;
            if (io.teaql.data.utils.StrUtil.isNotEmpty(path) && cn.hutool.core.io.FileUtil.exist(path)) {
                jsonStr = cn.hutool.core.io.FileUtil.readUtf8String(path);
            } else {
                jsonStr = io.teaql.data.utils.ResourceUtil.readUtf8Str("teaql-i18n.json");
            }
            i18nDict = cn.hutool.json.JSONUtil.parseObj(jsonStr);
        } catch (Exception e) {
            i18nDict = new cn.hutool.json.JSONObject();
        }
        loaded = true;
    }

    public BaseLanguageTranslator() {
        String langKey = getLanguageKey();
        if (!"en".equals(langKey)) {
            String path = System.getProperty("teaql.i18n.path");
            if (io.teaql.data.utils.StrUtil.isEmpty(path)) {
                throw new IllegalStateException("Translation dictionary is required for non-English locale '" + langKey 
                    + "'. Please configure the JVM parameter -Dteaql.i18n.path pointing to the translated JSON file.");
            }
            if (!cn.hutool.core.io.FileUtil.exist(path)) {
                throw new IllegalStateException("The configured translation dictionary file at '" + path 
                    + "' does not exist. Please check the JVM parameter -Dteaql.i18n.path.");
            }
            loadDict();
            if (i18nDict == null || i18nDict.isEmpty()) {
                throw new IllegalStateException("The translation dictionary file at '" + path 
                    + "' could not be loaded or is empty.");
            }
        } else {
            loadDict();
        }
    }

    protected String getLanguageKey() {
        String className = this.getClass().getSimpleName();
        if (className.endsWith("Translator")) {
            String name = className.substring(0, className.length() - "Translator".length());
            switch (name) {
                case "Arabic": return "ar";
                case "Chinese": return "zh_CN";
                case "TraditionalChinese": return "zh_TW";
                case "Spanish": return "es";
                case "French": return "fr";
                case "German": return "de";
                case "Japanese": return "ja";
                case "Korean": return "ko";
                case "Portuguese": return "pt";
                case "Thai": return "th";
                case "Ukrainian": return "uk";
                case "Filipino": return "fil";
                case "Indonesian": return "id";
                case "English": return "en";
            }
        }
        return "en";
    }

    protected String lookupTranslation(String term, String languageKey) {
        loadDict();
        if (i18nDict == null || term == null || languageKey == null) {
            return null;
        }
        cn.hutool.json.JSONObject termObj = i18nDict.getJSONObject(term);
        if (termObj != null) {
            return termObj.getStr(languageKey);
        }
        return null;
    }

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
            String member = ((HashLocation) location).getMember();
            String translation = lookupTranslation(member, getLanguageKey());
            if (translation != null) {
                return translation;
            }
            return convertToTitleCase(member);
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
