package io.teaql.data.sql;

import io.teaql.data.Repository;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.EntityMetaFactory;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.Relation;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class SQLRepositorySchemaHelper {

  // ensure 所有的表
  public void ensureSchema(UserContext ctx, EntityMetaFactory entityMetaFactory) {
    List<EntityDescriptor> entityDescriptors = entityMetaFactory.allEntityDescriptors();
    Set<EntityDescriptor> handled = new HashSet<>();
    for (EntityDescriptor entityDescriptor : entityDescriptors) {
      ensureSchema(ctx, handled, entityDescriptor);
    }
  }

  // ensure这个itemDescriptor以及依赖的所有表
  public void ensureSchema(UserContext ctx, EntityDescriptor entityDescriptor) {
    ensureSchema(ctx, new HashSet<>(), entityDescriptor);
  }

  public void ensureSchema(UserContext ctx, String entityType) {
    Repository repository = ctx.resolveRepository(entityType);
    EntityDescriptor entityDescriptor = repository.getEntityDescriptor();
    ensureSchema(ctx, entityDescriptor);
  }

  private void ensureSchema(
      UserContext ctx, Set<EntityDescriptor> handled, EntityDescriptor entityDescriptor) {
    if (handled.contains(entityDescriptor)) {
      return;
    }
    handled.add(entityDescriptor);
    EntityDescriptor parent = entityDescriptor.getParent();
    // parent 存在时ensure parent
    if (parent != null) {
      ensureSchema(ctx, handled, parent);
    }
    // 我持有的所有relation, 先解析引用的
    List<Relation> ownRelations = entityDescriptor.getOwnRelations();
    for (Relation ownRelation : ownRelations) {
      PropertyDescriptor reverseProperty = ownRelation.getReverseProperty();
      EntityDescriptor owner = reverseProperty.getOwner();
      ensureSchema(ctx, handled, owner);
    }
    // 解析自已
    String type = entityDescriptor.getType();
    Repository repository = ctx.resolveRepository(type);

    // 只有是sqlRepository才能ensure
    if (repository instanceof SQLRepository) {
      ((SQLRepository<?>) repository).ensureSchema(ctx);
    }
  }
}
