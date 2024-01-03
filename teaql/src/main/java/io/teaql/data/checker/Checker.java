package io.teaql.data.checker;

import cn.hutool.core.util.NumberUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import io.teaql.data.BaseEntity;
import io.teaql.data.UserContext;
import java.time.LocalDateTime;

/** check or set (default) values for the entity before persist */
public interface Checker<T extends BaseEntity> {
  String TEAQL_DATA_CHECK_RESULT = "teaql_data_check_result";
  String TEAQL_DATA_CHECKED_ITEMS = "teaql_data_checkedItems";

  void checkAndFix(UserContext ctx, T entity, ObjectLocation location);

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

  default ObjectLocation newLocation(ObjectLocation parent, String member) {
    if (ObjectUtil.isEmpty(parent)) {
      return ObjectLocation.hashRoot(member);
    }
    return parent.member(member);
  }

  default ObjectLocation newLocation(ObjectLocation parent, String member, int index) {
    return newLocation(parent, member).element(index);
  }

  default void requiredCheck(UserContext ctx, ObjectLocation location, Object current) {
    if (ObjectUtil.isNull(current)) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.required(location));
    }
  }

  default void minNumberCheck(
      UserContext ctx, ObjectLocation location, Number minNumber, Number current) {
    if (NumberUtil.isLess(NumberUtil.toBigDecimal(current), NumberUtil.toBigDecimal(minNumber))) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.min(location, minNumber, current));
    }
  }

  default void maxNumberCheck(
      UserContext ctx, ObjectLocation location, Number maxNumber, Number current) {
    if (NumberUtil.isGreater(
        NumberUtil.toBigDecimal(current), NumberUtil.toBigDecimal(maxNumber))) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.max(location, maxNumber, current));
    }
  }

  default void minStringCheck(
      UserContext ctx, ObjectLocation location, int minLen, CharSequence value) {
    if (StrUtil.length(value) < minLen) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.minStr(location, minLen, value));
    }
  }

  default void maxStringCheck(
      UserContext ctx, ObjectLocation location, int maxLen, CharSequence value) {
    if (StrUtil.length(value) > maxLen) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.maxStr(location, maxLen, value));
    }
  }

  default void minDateTimeCheck(
      UserContext ctx, ObjectLocation location, LocalDateTime minDate, LocalDateTime value) {
    if (value.isBefore(minDate)) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.minDate(location, minDate, value));
    }
  }

  default void maxDateTimeCheck(
      UserContext ctx, ObjectLocation location, LocalDateTime maxDate, LocalDateTime value) {
    if (value.isAfter(maxDate)) {
      ctx.append(TEAQL_DATA_CHECK_RESULT, CheckResult.maxDate(location, maxDate, value));
    }
  }

  default void checkAndFix(UserContext ctx, T entity) {
    checkAndFix(ctx, entity, null);
  }
}
