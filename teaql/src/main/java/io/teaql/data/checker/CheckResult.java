package io.teaql.data.checker;

import java.time.LocalDateTime;

public class CheckResult {
  private RuleId ruleId;
  private ObjectLocation location;

  private String rootType;

  private Object inputValue;
  private Object systemValue;

  private String naturalLanguageStatement;

  public static CheckResult required(ObjectLocation location) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(location);
    checkResult.setRuleId(RuleId.REQUIRED);
    return checkResult;
  }

  public static Object min(ObjectLocation location, Number minNumber, Number current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(location);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(minNumber);
    checkResult.setRuleId(RuleId.MIN);
    return checkResult;
  }

  public static Object max(ObjectLocation location, Number maxNumber, Number current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(location);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(maxNumber);
    checkResult.setRuleId(RuleId.MAX);
    return checkResult;
  }

  public static Object minStr(ObjectLocation location, int minLen, CharSequence current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(location);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(minLen);
    checkResult.setRuleId(RuleId.MIN_STR_LEN);
    return checkResult;
  }

  public static Object maxStr(ObjectLocation location, int maxLen, CharSequence current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(location);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(maxLen);
    checkResult.setRuleId(RuleId.MAX_STR_LEN);
    return checkResult;
  }

  public static Object minDate(ObjectLocation location, LocalDateTime min, LocalDateTime current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(location);
    checkResult.setInputValue(current);
    checkResult.setSystemValue(min);
    checkResult.setRuleId(RuleId.MIN_DATE);
    return checkResult;
  }

  public static Object maxDate(ObjectLocation location, LocalDateTime max, LocalDateTime current) {
    CheckResult checkResult = new CheckResult();
    checkResult.setLocation(location);
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

  public ObjectLocation getLocation() {
    return location;
  }

  public void setLocation(ObjectLocation pLocation) {
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

  @Override
  public String toString() {
    return "CheckResult{"
        + "ruleId="
        + ruleId
        + ", location="
        + location
        + ", inputValue="
        + inputValue
        + ", systemValue="
        + systemValue
        + '}';
  }

  public String getNaturalLanguageStatement() {
    return naturalLanguageStatement;
  }

  public void setNaturalLanguageStatement(String pNaturalLanguageStatement) {
    naturalLanguageStatement = pNaturalLanguageStatement;
  }
}
