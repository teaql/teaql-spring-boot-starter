package io.teaql.data.jackson;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import io.teaql.data.SmartList;
import java.io.IOException;
import java.util.List;

public class SmartListAsListSerializer extends StdSerializer<SmartList> {
  protected SmartListAsListSerializer(Class<SmartList> t) {
    super(t);
  }

  @Override
  public void serialize(SmartList value, JsonGenerator gen, SerializerProvider provider)
      throws IOException {
    List data = value.getData();
    gen.writePOJO(data);
  }
}
