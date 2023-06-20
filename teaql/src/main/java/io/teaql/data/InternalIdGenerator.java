package io.teaql.data;

public interface InternalIdGenerator {

  Long generateId(Entity baseEntity);
}
