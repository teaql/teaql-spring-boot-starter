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

  // ensure all tables of all the repositories
  public void ensureSchema(UserContext ctx, EntityMetaFactory entityMetaFactory) {
    List<EntityDescriptor> entityDescriptors = entityMetaFactory.allEntityDescriptors();
    Set<EntityDescriptor> handled = new HashSet<>();
    for (EntityDescriptor entityDescriptor : entityDescriptors) {
      ensureSchema(ctx, handled, entityDescriptor);
    }
  }

  // ensure table schema of the entity, including its dependencies
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
    // parent not null, ensure parent
    if (parent != null) {
      ensureSchema(ctx, handled, parent);
    }
    // the relation dependencies
    List<Relation> ownRelations = entityDescriptor.getOwnRelations();
    for (Relation ownRelation : ownRelations) {
      PropertyDescriptor reverseProperty = ownRelation.getReverseProperty();
      EntityDescriptor owner = reverseProperty.getOwner();
      ensureSchema(ctx, handled, owner);
    }

    if (!entityDescriptor.hasRepository()) {
      return;
    }

    // ensure self
    String type = entityDescriptor.getType();
    Repository repository = ctx.resolveRepository(type);

    // now only sql repository need to ensure schema
    if (repository instanceof SQLRepository) {
      ((SQLRepository<?>) repository).ensureSchema(ctx);
    }
  }
}
