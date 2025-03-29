package io.teaql.data;

import java.util.List;

import io.teaql.data.checker.CheckResult;

public interface NaturalLanguageTranslator {
    List<CheckResult> translateError(Entity pEntity, List<CheckResult> errors);
}
