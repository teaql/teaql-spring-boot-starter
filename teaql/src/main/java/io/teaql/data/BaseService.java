package io.teaql.data;

import cn.hutool.core.map.MapUtil;
import cn.hutool.core.util.ClassUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.core.util.StrUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.teaql.data.criteria.Operator;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.Relation;
import io.teaql.data.web.WebAction;
import io.teaql.data.web.WebResponse;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * a generic service implementation for entity CRUD operations
 *
 * @author jackytian
 */
public abstract class BaseService {

  /**
   * the main service entrance
   *
   * @param ctx user context
   * @param action action name
   * @param parameter parameter, json format
   * @return webResponse
   */
  public final WebResponse execute(
      UserContext ctx, String beanName, String action, String parameter) {
    if (ObjectUtil.isEmpty(action)) {
      return WebResponse.fail("missing action");
    }
    Method method =
        ReflectUtil.getPublicMethod(this.getClass(), action, UserContext.class, String.class);
    if (method != null) {
      return ReflectUtil.invoke(this, method, ctx, parameter);
    }
    if (action.startsWith("search")) {
      return doDynamicSearch(ctx, beanName, action, parameter);
    }
    if (action.startsWith("save")) {
      return doSave(ctx, action, parameter);
    }
    if (action.startsWith("delete")) {
      return doDelete(ctx, action, parameter);
    }
    if (action.startsWith("list") && action.endsWith("ForCandidate")) {
      return doCandidate(ctx, action, parameter);
    }
    return WebResponse.fail(StrUtil.format("unknown action: %s", action));
  }

  private WebResponse doCandidate(UserContext ctx, String action, String parameter) {
    String type = StrUtil.removePrefix(action, "list");
    type = StrUtil.removeSuffix(type, "ForCandidate");
    Class<? extends BaseRequest> requestClass = requestClass(type);
    Class<? extends BaseEntity> entityClass = getEntityClass(type);
    BaseRequest baseRequest = ReflectUtil.newInstance(requestClass, entityClass);
    baseRequest.selectAll();
    if (ObjectUtil.isNotEmpty(parameter)) {
      baseRequest.internalFindWithJsonExpr(parameter);
    }
    return WebResponse.of(baseRequest.executeForList(ctx));
  }

  private WebResponse doDelete(UserContext ctx, String action, String parameter) {
    String type = StrUtil.removePrefix(action, "delete");
    Entity entity = reloadEntity(ctx, type, parameter);
    if (entity == null) {
      return WebResponse.success();
    }
    entity.markAsDeleted();
    entity.save(ctx);
    return WebResponse.success();
  }

  private WebResponse doSave(UserContext ctx, String action, String parameter) {
    String type = StrUtil.removePrefix(action, "save");
    BaseEntity baseEntity = parseEntity(ctx, parameter, type);
    if (baseEntity == null) {
      return WebResponse.success();
    }
    Long id = baseEntity.getId();
    if (id == null) {
      setContextRelationBeforeSave(ctx, type, baseEntity);
      baseEntity.save(ctx);
    } else {
      // update
      BaseEntity dbItem = reloadEntity(ctx, type, parameter);
      if (dbItem == null) {
        throw new TQLException(StrUtil.format("item [{}] with id [{}] does not exist", type, id));
      }
      EntityDescriptor entityDescriptor = ctx.resolveEntityDescriptor(type);
      mergeEntity(ctx, entityDescriptor, baseEntity, dbItem);
      // save item
      dbItem.save(ctx);
    }
    return WebResponse.success();
  }

  private void mergeEntity(
      UserContext ctx, EntityDescriptor entityDescriptor, Entity baseEntity, Entity dbItem) {
    // try update Simple properties
    List<PropertyDescriptor> ownProperties = entityDescriptor.getOwnProperties();
    for (PropertyDescriptor ownProperty : ownProperties) {
      String name = ownProperty.getName();
      Object property = baseEntity.getProperty(name);
      ReflectUtil.invoke(dbItem, StrUtil.upperFirstAndAddPre("update", name), property);
    }

    // try update relation
    List<Relation> ownRelations = entityDescriptor.getOwnRelations();
    for (Relation ownRelation : ownRelations) {
      String name = ownRelation.getName();
      BaseEntity r = baseEntity.getProperty(name);
      if (r != null) {
        r.set$status(EntityStatus.REFER);
      }
      ReflectUtil.invoke(dbItem, StrUtil.upperFirstAndAddPre("update", name), r);
    }

    // try update attached relationships
    List<Relation> foreignRelations = entityDescriptor.getForeignRelations();
    for (Relation foreignRelation : foreignRelations) {
      String name = foreignRelation.getName();
      PropertyDescriptor reverseProperty = foreignRelation.getReverseProperty();
      Boolean attach = MapUtil.getBool(reverseProperty.getAdditionalInfo(), "attach");
      if (attach != null && attach) {
        SmartList<? extends Entity> children = baseEntity.getProperty(name);
        SmartList<? extends Entity> currentChildren = dbItem.getProperty(name);
        Map<Long, ? extends Entity> identityMap = currentChildren.toIdentityMap(Entity::getId);
        for (Entity child : children) {
          Long childId = child.getId();
          // for new child
          if (childId == null) {
            dbItem.addRelation(name, child);
          } else {
            Entity entity = identityMap.get(childId);
            if (entity == null) {
              dbItem.addRelation(name, child);
            } else {
              mergeEntity(ctx, ctx.resolveEntityDescriptor(entity.typeName()), child, entity);
            }
          }
        }
      }
    }
  }

  protected void setContextRelationBeforeSave(UserContext ctx, String type, BaseEntity baseEntity) {
    Relation contextRelation = getContextRelation(ctx, type);
    if (contextRelation != null) {
      Object merchant = ReflectUtil.invoke(ctx, "getMerchant");
      ReflectUtil.invoke(
          baseEntity, StrUtil.upperFirstAndAddPre("update", contextRelation.getName()), merchant);
    }
  }

  private BaseEntity reloadEntity(UserContext ctx, String type, String parameter) {
    BaseEntity baseEntity = parseEntity(ctx, type, parameter);
    Long id = baseEntity.getId();
    if (id == null) {
      return null;
    }
    Class<BaseRequest> baseRequestClass = requestClass(type);
    BaseRequest baseRequest = ReflectUtil.newInstance(baseRequestClass, getEntityClass(type));
    baseRequest.appendSearchCriteria(
        baseRequest.createBasicSearchCriteria(BaseEntity.ID_PROPERTY, Operator.EQUAL, id));

    EntityDescriptor entityDescriptor = ctx.resolveEntityDescriptor(type);
    List<Relation> foreignRelations = entityDescriptor.getForeignRelations();
    for (Relation foreignRelation : foreignRelations) {
      String name = foreignRelation.getName();
      PropertyDescriptor reverseProperty = foreignRelation.getReverseProperty();
      Boolean attach = MapUtil.getBool(reverseProperty.getAdditionalInfo(), "attach");
      if (attach != null && attach) {
        ReflectUtil.invoke(baseRequest, StrUtil.upperFirstAndAddPre(name, "select"));
      }
    }
    return (BaseEntity) baseRequest.execute(ctx);
  }

  private BaseEntity parseEntity(UserContext ctx, String type, String parameter) {
    if (ObjectUtil.isEmpty(parameter)) {
      throw new IllegalArgumentException("missing parameter");
    }
    ctx.resolveEntityDescriptor(type);
    Class<? extends BaseEntity> entityClass = getEntityClass(type);
    ObjectMapper objectMapper = ctx.getBean(ObjectMapper.class);
    try {
      return objectMapper.readValue(parameter, entityClass);
    } catch (JsonProcessingException pE) {
      throw new TQLException(pE);
    }
  }

  private WebResponse doDynamicSearch(
      UserContext ctx, String beanName, String action, String parameter) {
    String type = StrUtil.removePrefix(action, "search");
    Class<? extends BaseRequest> requestClass = requestClass(type);
    Class<? extends BaseEntity> entityClass = getEntityClass(type);
    BaseRequest baseRequest = ReflectUtil.newInstance(requestClass, entityClass);
    baseRequest.selectAll();
    if (ObjectUtil.isNotEmpty(parameter)) {
      baseRequest.internalFindWithJsonExpr(parameter);
    }
    addContextRelationFilter(ctx, type, baseRequest);

    EntityDescriptor entityDescriptor = ctx.resolveEntityDescriptor(type);
    List<Relation> foreignRelations = entityDescriptor.getForeignRelations();
    List<String> dynamicProperties = new ArrayList<>();
    for (Relation foreignRelation : foreignRelations) {
      String name = foreignRelation.getName();
      String retName = StrUtil.upperFirstAndAddPre(name, "sizeOf");
      dynamicProperties.add(retName);
      PropertyDescriptor reverseProperty = foreignRelation.getReverseProperty();
      EntityDescriptor owner = reverseProperty.getOwner();
      String subType = owner.getType();
      Class<? extends BaseRequest> subRequestClass = requestClass(subType);
      Class<? extends BaseEntity> subEntityClass = getEntityClass(subType);
      BaseRequest subRequest = ReflectUtil.newInstance(subRequestClass, subEntityClass);
      subRequest.unlimited().count();
      subRequest.setPartitionProperty(reverseProperty.getName());
      baseRequest.addAggregateDynamicProperty(retName, subRequest, true);
      // load attached list
      Boolean attach = MapUtil.getBool(reverseProperty.getAdditionalInfo(), "attach");
      if (attach != null && attach) {
        ReflectUtil.invoke(baseRequest, StrUtil.upperFirstAndAddPre(name, "select"));
      }
    }

    SmartList<? extends BaseEntity> ret = baseRequest.executeForList(ctx);
    for (BaseEntity entity : ret) {
      entity.addAction(WebAction.modifyWebAction(saveURL(beanName, type)));
      Long subCounts = entity.sumDynaPropOfNumberAsLong(dynamicProperties);
      if (subCounts == 0) {
        entity.addAction(WebAction.modifyWebAction(deleteURL(beanName, type)));
      }
    }
    return WebResponse.of(ret);
  }

  private void addContextRelationFilter(UserContext ctx, String type, BaseRequest baseRequest) {
    Relation contextRelation = getContextRelation(ctx, type);
    if (contextRelation != null) {
      baseRequest.appendSearchCriteria(
          baseRequest.createBasicSearchCriteria(
              contextRelation.getName(), Operator.EQUAL, ReflectUtil.invoke(ctx, "getMerchant")));
    }
  }

  private Relation getContextRelation(UserContext ctx, String type) {
    EntityDescriptor entityDescriptor = ctx.resolveEntityDescriptor(type);
    List<Relation> ownRelations = entityDescriptor.getOwnRelations();
    for (Relation ownRelation : ownRelations) {
      Boolean context = MapUtil.getBool(ownRelation.getAdditionalInfo(), "context");
      if (context != null && context) {
        return ownRelation;
      }
    }
    return null;
  }

  private Class<BaseEntity> getEntityClass(String type) {
    return ClassUtil.loadClass(StrUtil.format("{}.{}.{}", rootPackage(), type.toLowerCase(), type));
  }

  private Class<BaseRequest> requestClass(String type) {
    return ClassUtil.loadClass(
        StrUtil.format("{}.{}.{}Request", rootPackage(), type.toLowerCase(), type));
  }

  private String rootPackage() {
    Class clazz = this.getClass();
    while (clazz != null) {
      if (BaseService.class.equals(clazz.getSuperclass())) {
        return clazz.getPackage().getName();
      }
      clazz = clazz.getSuperclass();
    }
    throw new TQLException("cannot guess the domain package");
  }

  protected String saveURL(String beanName, String objectType) {
    return StrUtil.format("{}/save{}", beanName, objectType);
  }

  protected String deleteURL(String beanName, String objectType) {
    return StrUtil.format("{}/delete{}", beanName, objectType);
  }
}
