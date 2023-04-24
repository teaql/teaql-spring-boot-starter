package io.teaql.data.meta;

import java.util.List;

/** 所有的entity元信息注册或解析 */
public interface EntityMetaFactory {
  EntityDescriptor resolveEntityDescriptor(String type);

  void register(EntityDescriptor type);

  List<EntityDescriptor> allEntityDescriptors();
}
