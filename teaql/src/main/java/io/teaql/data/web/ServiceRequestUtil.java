package io.teaql.data.web;

import static io.teaql.data.web.UITemplateRender.serviceRequestPopupKey;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.*;
import io.teaql.data.*;
import io.teaql.data.criteria.Operator;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.Relation;
import java.lang.reflect.Method;
import java.util.*;
import java.util.function.Predicate;

public class ServiceRequestUtil {

  public static final String SERVICE_REQUEST = "serviceRequestDescriptor";

  public static <T extends BaseEntity> List<T> getDBViews(UserContext ctx, T view) {
    if (view == null) {
      return Collections.emptyList();
    }
    reloadRequest(ctx, view);
    BaseEntity serviceRequest = getServiceRequest(ctx, view);
    Relation serviceRequestRelation = getServiceRequestRelation(ctx, view);
    List<T> dbViews =
        serviceRequest.getProperty(serviceRequestRelation.getReverseProperty().getName());
    if (dbViews != null) {
      for (T dbView : dbViews) {
        dbView.setProperty(serviceRequestRelation.getName(), serviceRequest);
      }
    }
    return dbViews;
  }

  private static <T extends BaseEntity> Relation getServiceRequestRelation(
      UserContext ctx, T view) {
    EntityDescriptor entityDescriptor = ctx.resolveEntityDescriptor(view.typeName());
    List<Relation> properties = entityDescriptor.getOwnRelations();
    for (Relation relation : properties) {
      String isServiceRequest =
          relation
              .getReverseProperty()
              .getOwner()
              .getAdditionalInfo()
              .getOrDefault(SERVICE_REQUEST, "false");
      if (BooleanUtil.toBoolean(isServiceRequest)) {
        return relation;
      }
    }
    return null;
  }

  public static <T> T getFirst(UserContext ctx, BaseEntity view, String property) {
    List<BaseEntity> dbViews = getDBViews(ctx, view);
    for (BaseEntity dbView : dbViews) {
      Object o = dbView.getProperty(property);
      if (o != null) {
        return (T) o;
      }
    }
    return null;
  }

  public static <T> T getLast(UserContext ctx, BaseEntity view, String property) {
    List<T> list = getList(ctx, view, property);
    if (CollectionUtil.isEmpty(list)) {
      return null;
    }
    return CollectionUtil.getLast(list);
  }

  public static <T extends BaseEntity> T getLast(UserContext ctx, T view) {
    List<T> list = getDBViews(ctx, view);
    if (CollectionUtil.isEmpty(list)) {
      return null;
    }
    return CollectionUtil.getLast(list);
  }

  public static <T extends BaseEntity> T getFirst(UserContext ctx, T view) {
    List<BaseEntity> dbViews = getDBViews(ctx, view);
    if (dbViews.isEmpty()) {
      return null;
    }
    return (T) dbViews.get(0);
  }

  public static <T> List<T> getList(UserContext ctx, BaseEntity view, String property) {
    List<BaseEntity> dbViews = getDBViews(ctx, view);

    List<T> ret = new ArrayList<>();
    for (BaseEntity dbView : dbViews) {
      Object o = dbView.getProperty(property);
      if (o != null) {
        ret.add((T) o);
      }
    }
    return ret;
  }

  public static <T extends BaseEntity> T reloadRequest(UserContext ctx, T view) {
    if (view == null) {
      return null;
    }
    Relation serviceRequestRelation = getServiceRequestRelation(ctx, view);
    if (serviceRequestRelation == null) {
      throw new TQLException("no service request defined on the view:" + view.typeName());
    }

    BaseEntity request = view.getProperty(serviceRequestRelation.getName());
    if (request == null) {
      return view;
    }

    if (request.get$status().equals(EntityStatus.REFER)) {
      Class<? extends Entity> targetType =
          serviceRequestRelation.getReverseProperty().getOwner().getTargetType();
      BaseRequest loadRequest =
          ReflectUtil.newInstance(
              ClassUtil.loadClass(StrUtil.format("{}Request", targetType.getName())), targetType);
      loadRequest.selectSelf();
      loadRequest.appendSearchCriteria(
          loadRequest.createBasicSearchCriteria(
              BaseEntity.ID_PROPERTY, Operator.EQUAL, request.getId()));
      request = (BaseEntity) loadRequest.execute(ctx);
      view.setProperty(serviceRequestRelation.getName(), request);
    }
    return view;
  }

  public static BaseEntity getServiceRequest(UserContext ctx, BaseEntity view) {
    if (view == null) {
      return null;
    }
    Relation serviceRequestRelation = getServiceRequestRelation(ctx, view);
    if (serviceRequestRelation == null) {
      throw new TQLException("no service request defined on the view:" + view.typeName());
    }
    return view.getProperty(serviceRequestRelation.getName());
  }

  // save request on the view
  public static <T extends BaseEntity> T saveRequest(
      UserContext ctx, T view, ViewOption viewOption) {
    if (view == null || viewOption == null) {
      return null;
    }
    BaseEntity request = getServiceRequest(ctx, view);
    if (request == null) {
      return view;
    }

    if (viewOption.needReload()) {
      reloadRequest(ctx, view);
      request = getServiceRequest(ctx, view);
    }

    Relation serviceRequestRelation = getServiceRequestRelation(ctx, view);
    String name = serviceRequestRelation.getName();
    if (viewOption.isSaveView()) {
      if (viewOption.isOverride()) {
        request.setProperty(serviceRequestRelation.getReverseProperty().getName(), null);
      }
      view.setProperty(name, null);
      request.addRelation(serviceRequestRelation.getReverseProperty().getName(), view);
    }

    if (viewOption.isSave()) {
      saveRequestInView(ctx, request);
    }
    view.setProperty(name, request);

    if (!viewOption.isFirst()) {
      return view;
    }

    SmartList l = request.getProperty(serviceRequestRelation.getReverseProperty().getName());
    Entity first = l.first();
    if (first != null) {
      first.setProperty(name, request);
    }
    return (T) first;
  }

  private static void saveRequestInView(UserContext ctx, BaseEntity request) {
    String property = findJsonMeProperty(ctx, request);
    // update the jsonMe property to null, and trigger save for the request
    Method method =
        ReflectUtil.getMethodByName(
            request.getClass(), StrUtil.upperFirstAndAddPre(property, "update"));
    ReflectUtil.invoke(request, method, null);
    request.save(ctx);
  }

  private static <T extends BaseEntity> String findJsonMeProperty(UserContext ctx, T request) {
    EntityDescriptor serviceRequestDescriptor = ctx.resolveEntityDescriptor(request.typeName());
    List<PropertyDescriptor> properties = serviceRequestDescriptor.getProperties();
    for (PropertyDescriptor propertyDescriptor : properties) {
      if (propertyDescriptor.getClass().getSimpleName().equalsIgnoreCase("JsonMeProperty")) {
        return propertyDescriptor.getName();
      }
    }
    return null;
  }

  public static <T extends BaseEntity> T removeView(
      UserContext ctx, T view, String property, Object value) {
    return removeView(ctx, view, v -> ObjectUtil.equals(value, v.getProperty(property)));
  }

  public static <T extends BaseEntity> T clear(UserContext ctx, T view) {
    return removeView(ctx, view, x -> true);
  }

  public static <T extends BaseEntity> T removeView(UserContext ctx, T view, Predicate<T> filter) {
    if (view == null) {
      return null;
    }

    reloadRequest(ctx, view);
    Relation serviceRequestRelation = getServiceRequestRelation(ctx, view);
    BaseEntity request = getServiceRequest(ctx, view);
    SmartList list = request.getProperty(serviceRequestRelation.getReverseProperty().getName());
    if (list == null) {
      return view;
    }
    boolean updated = list.removeIf(filter);
    if (updated) {
      saveRequestInView(ctx, request);
    }
    view.setProperty(serviceRequestRelation.getName(), request);
    return view;
  }

  public static Object showPop(UserContext ctx, BaseEntity view) {
    if (view == null) {
      return null;
    }
    BaseEntity requestObj = getServiceRequest(ctx, view);
    if (requestObj == null) {
      return null;
    }
    return ctx.getAndRemoveInStore(serviceRequestPopupKey(requestObj));
  }

  public enum ViewOption {

    // Bit1：1 save request，0 read only
    // Bit2：1 save current view, 0 ignore current view
    // Bit3：1 override view, 0 append current view
    // Bit4: 1 return first view，0 return current view
    OVERRIDE_FIRST(0b1111),
    OVERRIDE_CURRENT(0b1110),
    APPEND_FIRST(0b1101),
    APPEND_CURRENT(0b1100),
    NO_SAVE_FIRST(0b0001),
    NO_SAVE_CURRENT(0b0000),
    SAVE_FIRST(0b1001),
    SAVE_CURRENT(0b1000);

    int code;

    ViewOption(int code) {
      this.code = code;
    }

    public boolean isOverride() {
      return (this.code & 0b10) != 0;
    }

    public boolean isSaveView() {
      return (this.code & 0b100) != 0;
    }

    public boolean isSave() {
      return (this.code & 0b1000) != 0;
    }

    public boolean isFirst() {
      return (this.code & 0b1) != 0;
    }

    public boolean needReload() {
      return isSave() || isFirst();
    }
  }
}
