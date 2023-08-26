package io.teaql.data;

import io.teaql.data.checker.CheckResult;
import java.util.List;

public interface NaturalLanguageTranslator {
  List<CheckResult> translateError(Entity pEntity, List<CheckResult> errors);
}
