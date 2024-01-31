package io.teaql.data.translation;

import cn.hutool.core.collection.CollStreamUtil;
import java.util.Map;
import java.util.Set;

public class TranslationResponse {
  private Set<TranslationRecord> records;

  public TranslationResponse(TranslationRequest req) {
    this.records = req.getRecords();
    for (TranslationRecord record : this.records) {
      record.setValue(record.getKey());
    }
  }

  public Map<String, String> getResults() {
    return CollStreamUtil.toMap(records, TranslationRecord::getKey, TranslationRecord::getValue);
  }

  public Set<TranslationRecord> getRecords() {
    return records;
  }

  public void setRecords(Set<TranslationRecord> pRecords) {
    records = pRecords;
  }
}
