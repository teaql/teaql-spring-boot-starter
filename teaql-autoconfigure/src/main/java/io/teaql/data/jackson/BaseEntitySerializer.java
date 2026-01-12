package io.teaql.data.jackson;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

import io.teaql.data.BaseEntity;

public class BaseEntitySerializer extends StdSerializer<BaseEntity> {
    protected BaseEntitySerializer(Class<BaseEntity> src) {
        super(src);
    }

    @Override
    public void serialize(BaseEntity value, JsonGenerator gen, SerializerProvider provider) throws IOException {

    }
}
