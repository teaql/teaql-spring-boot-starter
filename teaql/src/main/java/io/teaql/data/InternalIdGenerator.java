package io.teaql.data;

import com.fasterxml.jackson.databind.ser.Serializers;

public interface InternalIdGenerator {
    
    Long generateId(Entity baseEntity);

}
