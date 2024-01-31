package io.teaql.data.translation;

public interface Translator {
    TranslationResponse translate(TranslationRequest req);
}
