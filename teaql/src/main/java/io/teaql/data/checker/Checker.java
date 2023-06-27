package io.teaql.data.checker;

import cn.hutool.core.util.NumberUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import io.teaql.data.BaseEntity;
import io.teaql.data.UserContext;
import java.time.LocalDateTime;

/** 在保存entity之前会用checker来检查或设置一些默认值 */
public interface Checker<T extends BaseEntity> {
  String TEAQL_DATA_CHECK_RESULT = "teaql_data_check_result";
  String TEAQL_DATA_CHECKED_ITEMS = "teaql_data_checkedItems";

  void checkAndFix(UserContext ctx, T entity, String preFix);

  default void markAsChecked(UserContext ctx, T entity) {
    ctx.append(TEAQL_DATA_CHECKED_ITEMS, entity);
  }

  default boolean needCheck(UserContext ctx, T entity) {
    if (ObjectUtil.isNull(entity)) {
      return false;
    }

    if (ctx.hasObject(TEAQL_DATA_CHECKED_ITEMS, entity)) {
      return false;
    }

    switch (entity.get$status()) {
      case NEW:
      case UPDATED:
        return true;
      default:
        return false;
    }
  }

  default String newPrefix(String prefix, String member) {
    if (ObjectUtil.isEmpty(prefix)) {
      return member;
    }
    return prefix + "." + member;
  }

  default String newPrefix(String prefix, String member, int index) {
    return StrUtil.format("{}[{}]", newPrefix(prefix, member), index);
  }

  default void requiredCheck(UserContext ctx, String preFix, Object current) {
    if (ObjectUtil.isNull(current)) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.required(preFix));
    }
  }

  default void minNumberCheck(UserContext ctx, String preFix, Number minNumber, Number current) {
    if (NumberUtil.isLess(NumberUtil.toBigDecimal(current), NumberUtil.toBigDecimal(minNumber))) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.min(preFix, minNumber, current));
    }
  }

  default void maxNumberCheck(UserContext ctx, String preFix, Number maxNumber, Number current) {
    if (NumberUtil.isGreater(
        NumberUtil.toBigDecimal(current), NumberUtil.toBigDecimal(maxNumber))) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.max(preFix, maxNumber, current));
    }
  }

  default void minStringCheck(UserContext ctx, String preFix, int minLen, CharSequence value) {
    if (StrUtil.length(value) < minLen) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.minStr(preFix, minLen, value));
    }
  }

  default void maxStringCheck(UserContext ctx, String preFix, int maxLen, CharSequence value) {
    if (StrUtil.length(value) > maxLen) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.maxStr(preFix, maxLen, value));
    }
  }

  default void minDateTimeCheck(
      UserContext ctx, String preFix, LocalDateTime minDate, LocalDateTime value) {
    if (value.isBefore(minDate)) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.minDate(preFix, minDate, value));
    }
  }

  default void maxDateTimeCheck(
      UserContext ctx, String preFix, LocalDateTime maxDate, LocalDateTime value) {
    if (value.isAfter(maxDate)) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.maxDate(preFix, maxDate, value));
    }
  }

  default void checkAndFix(UserContext ctx, T entity) {
    checkAndFix(ctx, entity, "");
  }
}
