package io.teaql.data.meta;

import java.util.List;

/** entity meta factory */
public interface EntityMetaFactory {
  EntityDescriptor resolveEntityDescriptor(String type);

  void register(EntityDescriptor type);

  List<EntityDescriptor> allEntityDescriptors();
}
