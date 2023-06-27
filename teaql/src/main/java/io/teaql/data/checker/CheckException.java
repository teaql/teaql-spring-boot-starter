package io.teaql.data.checker;

import cn.hutool.core.util.StrUtil;
import java.util.ArrayList;
import java.util.List;

public class CheckException extends RuntimeException {

  List<CheckResult> violates = new ArrayList<>();

  public CheckException() {
    super();
  }

  public CheckException(String message) {
    super(message);
  }

  public CheckException(String message, Throwable cause) {
    super(message, cause);
  }

  public CheckException(Throwable cause) {
    super(cause);
  }

  protected CheckException(
      String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }

  public CheckException(List<CheckResult> pErrors) {
    this(StrUtil.join(";", pErrors));
    this.violates = pErrors;
  }
}
