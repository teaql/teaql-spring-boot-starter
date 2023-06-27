package io.teaql.data.checker;

import java.time.LocalDateTime;

public class CheckResult {
  private RuleId ruleId;
  private String location;

  private String rootType;

  private Object inputValue;
  private Object systemValue;

  public static CheckResult required(String prefix) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(prefix);
    checkResult.setRuleId(RuleId.REQUIRED);
    return checkResult;
  }

  public static Object min(String preFix, Number minNumber, Number current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(preFix);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(minNumber);
    checkResult.setRuleId(RuleId.MIN);
    return checkResult;
  }

  public static Object max(String preFix, Number maxNumber, Number current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(preFix);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(maxNumber);
    checkResult.setRuleId(RuleId.MAX);
    return checkResult;
  }

  public static Object minStr(String preFix, int minLen, CharSequence current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(preFix);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(minLen);
    checkResult.setRuleId(RuleId.MIN_STR_LEN);
    return checkResult;
  }

  public static Object maxStr(String preFix, int maxLen, CharSequence current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(preFix);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(maxLen);
    checkResult.setRuleId(RuleId.MAX_STR_LEN);
    return checkResult;
  }

  public static Object minDate(String preFix, LocalDateTime min, LocalDateTime current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(preFix);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(min);
    checkResult.setRuleId(RuleId.MIN_DATE);
    return checkResult;
  }

  public static Object maxDate(String preFix, LocalDateTime max, LocalDateTime current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(preFix);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(max);
    checkResult.setRuleId(RuleId.MAX_DATE);
    return checkResult;
  }

  public RuleId getRuleId() {
    return ruleId;
  }

  public void setRuleId(RuleId pRuleId) {
    ruleId = pRuleId;
  }

  public String getLocation() {
    return location;
  }

  public void setLocation(String pLocation) {
    location = pLocation;
  }

  public String getRootType() {
    return rootType;
  }

  public void setRootType(String pRootType) {
    rootType = pRootType;
  }

  public Object getInputValue() {
    return inputValue;
  }

  public void setInputValue(Object pInputValue) {
    inputValue = pInputValue;
  }

  public Object getSystemValue() {
    return systemValue;
  }

  public void setSystemValue(Object pSystemValue) {
    systemValue = pSystemValue;
  }

  public enum RuleId {
    MIN,
    MAX,
    MIN_STR_LEN,
    MAX_STR_LEN,
    MIN_DATE,
    MAX_DATE,
    REQUIRED
  }
}
