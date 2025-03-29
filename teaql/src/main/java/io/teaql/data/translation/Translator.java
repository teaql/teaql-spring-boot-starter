package io.teaql.data.translation;

public interface Translator {
  Translator NOOP = req -> null;

  TranslationResponse translate(TranslationRequest req);
}
