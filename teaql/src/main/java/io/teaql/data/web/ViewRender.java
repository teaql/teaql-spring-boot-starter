package io.teaql.data.web;

import static io.teaql.data.UserContext.X_CLASS;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.codec.Base64Encoder;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.lang.TypeReference;
import cn.hutool.core.map.MapUtil;
import cn.hutool.core.util.*;
import cn.hutool.json.JSONUtil;
import io.teaql.data.*;
import io.teaql.data.checker.CheckException;
import io.teaql.data.checker.CheckResult;
import io.teaql.data.checker.HashLocation;
import io.teaql.data.checker.ObjectLocation;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.Relation;
import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

public abstract class ViewRender {

  public static final String FORM = "form";
  public static final String LIST = "list";
  public static final String UI_ATTRIBUTE_PREFIX = "ui_";
  public static final String UI_FIELD_ACTION_SUFFIX = "_action";
  public static final String UI_PASS_THROUGH_ATTRIBUTE_PREFIX = "ui-";
  public static final String UI_CANDIDATE_ATTRIBUTE_PREFIX = "ui_candidate_";
  public static final String TEMPLATE = "$template";
  public static final String NUMBER_TYPE = "_number";
  public static final String BOOL_TYPE = "_bool";
  public static final String CTX = "ctx.";
  public static final String NO_VALIDATE_FIELD = "noValidateField";

  public abstract String getBeanName();

  public Object view(UserContext ctx, Object data) {
    if (data == null) {
      return null;
    }

    Method showPop = ReflectUtil.getMethodByName(getTemplateRender(ctx).getClass(), "showPop");
    if (showPop != null) {
      Object o = ReflectUtil.invoke(getTemplateRender(ctx), showPop, ctx, data);
      if (o != null) {
        return o;
      }
    }

    invokeBeforeView(ctx, data);
    EntityDescriptor meta = ctx.resolveEntityDescriptor(((Entity) data).typeName());
    Map<String, Object> page = new HashMap<>();
    switch (getPageType(meta)) {
      case FORM:
        renderAsForm(ctx, page, meta, data);
        break;
      case LIST:
        renderAsList(ctx, page, meta, data);
        break;
      default:
        return data;
    }
    return preRender(ctx, data, page);
  }

  public Object preRender(UserContext ctx, Object data, Object view) {
    if (data != null) {
      Object bean = ctx.getBean(ClassUtil.loadClass(data.getClass().getName() + "Processor"));
      return ReflectUtil.invoke(bean, "defaultPreRender", ctx, data, view);
    }
    return view;
  }

  private void renderAsList(UserContext ctx, Object page, EntityDescriptor meta, Object data) {
    addListClass(ctx, page, meta, data);
    addPageTitle(ctx, page, meta, data);
    addListData(ctx, page, meta, data);
    setUIPassThrough(meta, page);
  }

  private void addListData(UserContext ctx, Object page, EntityDescriptor meta, Object data) {
    for (PropertyDescriptor fieldMeta : meta.getProperties()) {
      List candidates = prepareCandidates(ctx, meta, fieldMeta, data);
      if (candidates != null) {
        for (Object candidate : candidates) {
          Object refinedCandidate = buildItemOfList(ctx, fieldMeta, candidate, data);
          Object id = getProperty(refinedCandidate, "id");
          if (ObjectUtil.isEmpty(id)) {
            continue;
          }
          addValue(page, "list", MapUtil.of("id", id));
          setValue(page, "dataContainer." + id, refinedCandidate);
        }
        setListMeta(ctx, page, fieldMeta, candidates, data);
        break;
      }
    }
  }

  private Object buildItemOfList(
      UserContext ctx, PropertyDescriptor fieldMeta, Object candidateItem, Object data) {
    boolean noCandidateMapping = getBoolean(fieldMeta, "candidate-without-mapping", false);
    if (noCandidateMapping) {
      return buildItemOfListDirectly(ctx, fieldMeta, candidateItem, data);
    }
    Map<String, Object> ret = new HashMap<>();
    String idProp = getCandidateProperty(ctx, fieldMeta, "id");
    Object idPropertyValue = getCandidateValue(ctx, candidateItem, idProp);
    if (ObjectUtil.isEmpty(candidateItem)) {
      return null;
    }
    setValue(ret, "id", idPropertyValue);

    addCandidateCommonProperty(ctx, fieldMeta, ret, candidateItem);

    fieldMeta
        .getAdditionalInfo()
        .forEach(
            (k, value) -> {
              String key = k;
              if (!key.startsWith(UI_ATTRIBUTE_PREFIX)) {
                return;
              }
              if (!key.endsWith(UI_FIELD_ACTION_SUFFIX)) {
                return;
              }
              addValue(
                  candidateItem,
                  "actionList",
                  buildSelectAction(ctx, data, value, fieldMeta.getName(), idPropertyValue));
            });
    return ret;
  }

  private Object buildItemOfListDirectly(
      UserContext ctx, PropertyDescriptor meta, Object candidate, Object data) {
    Map<String, Object> mapping = BeanUtil.beanToMap(candidate);
    Map<String, Object> ret = new HashMap<>();
    mapping.forEach(
        (k, v) -> {
          if ("actionList".equals(k)) {
            if (v == null) {
              return;
            }
            Iterable l = (Iterable) v;
            for (Object action : l) {
              addValue(
                  ret,
                  "actionList",
                  buildSelectAction(
                      ctx,
                      data,
                      String.valueOf(action),
                      (String) meta.getName(),
                      mapping.get("id")));
            }
            return;
          }

          char c = k.charAt(0);
          if (!CharUtil.isLetter(c)) {
            addValue(ret, "infoList", MapUtil.builder().put("title", k).put("value", v).build());
            addValue(ret, "infoList", null);
          } else {
            setValue(ret, k, v);
          }
        });

    return ret;
  }

  private void setListMeta(
      UserContext ctx, Object page, PropertyDescriptor meta, List candidates, Object data) {
    String nextPage = String.format("prepareNextPageUrlFor%s", StrUtil.upperFirst(meta.getName()));

    Object nextPageUrl = null;
    if (hasCustomized(ctx, data, nextPage)) {
      nextPageUrl = invokeCandidateAction(ctx, data, nextPage);
    }

    setValue(page, "listMeta.linkToUrl", nextPageUrl);
  }

  private void addListClass(UserContext ctx, Object page, EntityDescriptor meta, Object data) {
    ctx.setResponseHeader(X_CLASS, "com.terapico.appview.ListOfPage");
  }

  private Object getTemplateRender(UserContext ctx) {
    return ctx.getBean("templateRender");
  }

  private void invokeBeforeView(UserContext ctx, Object view) {
    Object bean = ctx.getBean(ClassUtil.loadClass(view.getClass().getName() + "Processor"));
    ReflectUtil.invoke(bean, "beforeView", ctx, view);
  }

  public Object renderAsForm(UserContext ctx, Object page, EntityDescriptor meta, Object data) {
    addFormClass(ctx, page, meta, data);
    addFormId(ctx, page, meta, data);
    addPageTitle(ctx, page, meta, data);
    addFormActions(ctx, page, meta, data);
    addFormFields(ctx, page, meta, data);
    setUIPassThrough(meta, page);
    addRequestId(ctx, page);
    return page;
  }

  private void addRequestId(UserContext ctx, Object page) {
    Object defaultGroup = ensureFormGroup(page, "default");
    addValue(
        defaultGroup,
        "fieldList",
        MapUtil.builder()
            .put("name", "_req")
            .put("label", "_req")
            .put("type", "text")
            .put("value", IdUtil.fastSimpleUUID())
            .put("hidden", true)
            .build());
  }

  private void addFormFields(UserContext ctx, Object page, EntityDescriptor meta, Object data) {
    meta.getProperties()
        .forEach(
            property -> {
              if (property.isId() || property.isVersion()) {
                return;
              }
              addFormField(ctx, page, meta, property.getName(), property, data);
            });
  }

  private void addFormField(
      UserContext ctx,
      Object page,
      EntityDescriptor meta,
      String fieldName,
      PropertyDescriptor fieldMeta,
      Object data) {
    Boolean ignored = getBoolean(fieldMeta, "ignore", false);
    if (ignored) {
      return;
    }
    String groupName = getStr(fieldMeta, "group", "default");
    Object group = ensureFormGroup(page, groupName);

    Object formField = createFormField(ctx, meta, fieldName, fieldMeta, data);
    if (formField == null) {
      return;
    }
    addValue(group, "fieldList", formField);
  }

  private String getStr(PropertyDescriptor property, String key, String defaultValue) {
    return property.getStr(UI_ATTRIBUTE_PREFIX + key, defaultValue);
  }

  private Object createFormField(
      UserContext ctx,
      EntityDescriptor pMeta,
      String pFieldName,
      PropertyDescriptor pFieldMeta,
      Object pData) {
    Object fieldValue = getFieldValue(pData, pFieldName, pFieldMeta);
    boolean ignoreIfNull = getBoolean(pFieldMeta, "ignore-if-null", false);
    if (ignoreIfNull && fieldValue == null) {
      return null;
    }
    Object currentFieldValue = format(ctx, pFieldMeta, fieldValue);
    List candidates = prepareCandidates(ctx, pMeta, pFieldMeta, pData);
    Object uiField =
        MapUtil.builder()
            .put("name", pFieldName)
            .put("label", pFieldMeta.getStr("zh_CN", pFieldName))
            .put(
                "type",
                pFieldMeta.getStr(
                    "ui-type", defaultFormFieldType(ctx, pFieldMeta, currentFieldValue)))
            .put(currentFieldValue != null, "value", currentFieldValue)
            .build();
    if (getBoolean(pFieldMeta, "no_label", false)) {
      setValue(uiField, "label", null);
    }

    if (candidates != null) {
      candidates = CollectionUtil.sub(candidates, 0, getCandidateLimit(pFieldMeta));
      List mappingCandidates =
          (List)
              candidates.stream()
                  .map(c -> buildCandidate(ctx, pFieldMeta, c, currentFieldValue))
                  .collect(Collectors.toList());

      renderCandidateValues(ctx, pFieldMeta, fieldValue, candidates, mappingCandidates, uiField);
    }
    renderFieldValue(ctx, uiField, pMeta, pFieldMeta, fieldValue);
    buildFieldActions(ctx, uiField, pMeta, pFieldMeta, pData);
    setUIPassThrough(pFieldMeta, uiField);
    return uiField;
  }

  private void renderCandidateValues(
      UserContext ctx,
      PropertyDescriptor pFieldMeta,
      Object pFieldValue,
      List candidates,
      List mappingCandidates,
      Object uiField) {
    String template = getCandidateProperty(ctx, pFieldMeta, TEMPLATE, null);
    if (template != null) {
      Method templateRender =
          ReflectUtil.getMethodByName(getTemplateRender(ctx).getClass(), template);
      if (templateRender == null) {
        return;
      }
      ReflectUtil.invoke(
          getTemplateRender(ctx),
          templateRender,
          ctx,
          pFieldMeta,
          uiField,
          pFieldValue,
          candidates,
          mappingCandidates);
      return;
    }
    String candidateContainer =
        getCandidateProperty(ctx, pFieldMeta, "$container", "candidateValues");
    setValue(uiField, candidateContainer, null);
    mappingCandidates.forEach(
        candidate -> {
          addValue(uiField, candidateContainer, candidate);
        });
  }

  public void renderFieldValue(
      UserContext ctx,
      Object uiField,
      EntityDescriptor pMeta,
      PropertyDescriptor pFieldMeta,
      Object fieldValue) {
    String template = getStr(pFieldMeta, TEMPLATE, null);
    if (template != null) {
      Method templateRender =
          ReflectUtil.getMethodByName(getTemplateRender(ctx).getClass(), template);
      if (templateRender == null) {
        return;
      }
      ReflectUtil.invoke(
          getTemplateRender(ctx), templateRender, ctx, pFieldMeta, uiField, fieldValue);
    }
  }

  private Object getFieldValue(Object data, String fieldName, PropertyDescriptor meta) {
    if (data == null) {
      return null;
    }
    String vpath = getStr(meta, "vpath", fieldName);

    return getEnhancedProperty(data, vpath);
  }

  private Object getEnhancedProperty(Object data, String vpath) {
    if (data == null) {
      return null;
    }

    String[] vpaths = vpath.split("\\.");
    return getEnhancedProperty(data, vpaths);
  }

  private Object getEnhancedProperty(Object data, String[] vpath) {
    if (vpath == null || vpath.length == 0) {
      return data;
    }
    String property = vpath[0];
    return getEnhancedProperty(getProperty(data, property), ArrayUtil.sub(vpath, 1, vpath.length));
  }

  private void buildFieldActions(
      UserContext ctx,
      Object field,
      EntityDescriptor meta,
      PropertyDescriptor fieldMeta,
      Object pData) {
    fieldMeta
        .getAdditionalInfo()
        .forEach(
            (k, value) -> {
              String key = k;
              if (!key.startsWith(UI_ATTRIBUTE_PREFIX)) {
                return;
              }
              if (!key.endsWith(UI_FIELD_ACTION_SUFFIX)) {
                return;
              }
              setValue(
                  field,
                  StrUtil.removeSuffix(
                      StrUtil.removePrefix(key, UI_ATTRIBUTE_PREFIX), UI_FIELD_ACTION_SUFFIX),
                  createAction(ctx, pData, value));
            });
  }

  private Object buildCandidate(
      UserContext ctx, PropertyDescriptor meta, Object candidate, Object currentValue) {
    Map<String, Object> ret = new HashMap<>();
    String idProp = getCandidateProperty(ctx, meta, "id");
    Object idPropertyValue = getCandidateValue(ctx, candidate, idProp);
    Object currentIdPropertyValue = getCandidateValue(ctx, currentValue, idProp);
    setValue(ret, "id", idPropertyValue);
    if (currentIdPropertyValue != null && getBoolean(meta, "candidate_$select", true)) {
      setValue(ret, "selected", ObjectUtil.equals(idPropertyValue, currentIdPropertyValue));
    }
    addCandidateCommonProperty(ctx, meta, ret, candidate);
    return ret;
  }

  private void addCandidateCommonProperty(
      UserContext ctx, PropertyDescriptor meta, Object uiCandidate, Object candidateValue) {
    if (candidateValue == null) {
      return;
    }
    meta.getAdditionalInfo()
        .forEach(
            (k, v) -> {
              if (!k.startsWith(UI_CANDIDATE_ATTRIBUTE_PREFIX)) {
                return;
              }
              String key = StrUtil.removePrefix(k, UI_CANDIDATE_ATTRIBUTE_PREFIX);
              if (key.startsWith("$")) {
                return;
              }
              setValue(uiCandidate, key, getCandidateValue(ctx, candidateValue, v));
            });
  }

  private String getCandidateProperty(UserContext ctx, PropertyDescriptor meta, String property) {
    return getCandidateProperty(ctx, meta, property, property);
  }

  private String getCandidateProperty(
      UserContext ctx, PropertyDescriptor meta, String property, String defaultProperty) {
    return meta.getStr(UI_CANDIDATE_ATTRIBUTE_PREFIX + property, defaultProperty);
  }

  private Object getCandidateValue(UserContext ctx, Object value, String property) {
    String defaultValue = null;
    if (property.contains(":")) {
      defaultValue = property.substring(property.indexOf(":") + 1);
      property = property.substring(0, property.indexOf(":"));
    }
    if (value == null) {
      return defaultValue;
    }

    if (ClassUtil.isSimpleValueType(value.getClass())) {
      return value;
    }

    if (value instanceof BaseEntity && "displayName".equals(property)) {
      return ((BaseEntity) value).getDisplayName();
    }

    Object v = getEnhancedProperty(value, property);
    if (v == null) {
      return defaultValue;
    }
    return v;
  }

  private void setUIPassThrough(PropertyDescriptor meta, Object uiElement) {
    meta.getAdditionalInfo()
        .forEach(
            (k, value) -> {
              String key = k;
              if (!key.startsWith(UI_PASS_THROUGH_ATTRIBUTE_PREFIX)) {
                return;
              }
              String property = StrUtil.removePrefix(key, UI_PASS_THROUGH_ATTRIBUTE_PREFIX);

              if (StrUtil.endWith(property, NUMBER_TYPE)) {
                property = StrUtil.removeSuffix(property, NUMBER_TYPE);
                setValue(uiElement, property, NumberUtil.parseNumber((String) value));
                return;
              }

              if (StrUtil.endWith(property, BOOL_TYPE)) {
                property = StrUtil.removeSuffix(property, BOOL_TYPE);
                setValue(uiElement, property, BooleanUtil.toBoolean((String) value));
                return;
              }
              setValue(uiElement, property, value);
            });

    boolean optional = meta.getBoolean("optional", false);
    if (!optional) {
      setValue(uiElement, "required", "true");
    }
  }

  private void setUIPassThrough(EntityDescriptor meta, Object uiElement) {
    meta.getAdditionalInfo()
        .forEach(
            (k, value) -> {
              String key = k;
              if (!key.startsWith(UI_PASS_THROUGH_ATTRIBUTE_PREFIX)) {
                return;
              }
              String property = StrUtil.removePrefix(key, UI_PASS_THROUGH_ATTRIBUTE_PREFIX);

              if (StrUtil.endWith(property, NUMBER_TYPE)) {
                property = StrUtil.removeSuffix(property, NUMBER_TYPE);
                setValue(uiElement, property, NumberUtil.parseNumber((String) value));
                return;
              }

              if (StrUtil.endWith(property, BOOL_TYPE)) {
                property = StrUtil.removeSuffix(property, BOOL_TYPE);
                setValue(uiElement, property, BooleanUtil.toBoolean((String) value));
                return;
              }
              setValue(uiElement, property, value);
            });
  }

  private List prepareCandidates(
      UserContext pCtx, EntityDescriptor pMeta, PropertyDescriptor pFieldMeta, Object pData) {
    boolean ignoreCandidate = getBoolean(pFieldMeta, "no-candidate", false);
    if (ignoreCandidate) {
      return null;
    }

    String candidateAction =
        String.format("prepareCandidatesFor%s", StrUtil.upperFirst(pFieldMeta.getName()));

    if (hasCustomized(pCtx, pData, candidateAction)) {
      return invokeCandidateAction(pCtx, pData, candidateAction);
    }

    if (pFieldMeta instanceof Relation r) {
      return loadTop(pCtx, r);
    }
    return null;
  }

  private boolean hasCustomized(UserContext pCtx, Object pData, String pCandidateAction) {
    Object bean = pCtx.getBean(ClassUtil.loadClass(pData.getClass().getName() + "Processor"));
    Method method = ReflectUtil.getMethodOfObj(bean, pCandidateAction, pCtx, pData);
    return method != null;
  }

  private List loadConstants(Class pParentType) {
    return (List)
        ReflectUtil.getStaticFieldValue(ReflectUtil.getField(pParentType, "CODE_NAME_LIST"));
  }

  public List loadTop(UserContext pCtx, Relation meta) {
    if (meta == null) {
      return null;
    }
    EntityDescriptor parentType = meta.getReverseProperty().getOwner();
    BaseRequest request =
        ReflectUtil.newInstance(requestClass(parentType), getEntityClass(parentType));
    request.selectSelf();
    request.setSize(getCandidateLimit(meta));
    return ListUtil.toList(request.executeForList(pCtx));
  }

  private Class<? extends Entity> getEntityClass(EntityDescriptor descriptor) {
    return descriptor.getTargetType();
  }

  private Class<BaseRequest> requestClass(EntityDescriptor descriptor) {
    Class<? extends Entity> entityClass = getEntityClass(descriptor);
    return ClassUtil.loadClass(StrUtil.format("{}Request", entityClass.getName()));
  }

  private Integer getCandidateLimit(PropertyDescriptor meta) {
    return getInt(meta, "candidate-limit", 50);
  }

  private <T> T invokeCandidateAction(UserContext pCtx, Object pData, String pCandidateAction) {
    Object bean = pCtx.getBean(ClassUtil.loadClass(pData.getClass().getName() + "Processor"));
    return ReflectUtil.invoke(bean, pCandidateAction, pCtx, pData);
  }

  public String defaultFormFieldType(
      UserContext pCtx, PropertyDescriptor pFieldMeta, Object pCurrentFieldValue) {
    if (pFieldMeta instanceof Relation) {
      return "single-select";
    }

    if (pFieldMeta.getBoolean("isDate", false)) {
      return "datetime";
    }

    if (pFieldMeta.getBoolean("isInt", false)) {
      return "integer";
    }

    if (pFieldMeta.getBoolean("isNumber", false)) {
      return "double";
    }

    return "text";
  }

  public Object format(UserContext ctx, PropertyDescriptor meta, Object value) {
    String idProp = getCandidateProperty(ctx, meta, "id");
    return getCandidateValue(ctx, value, idProp);
  }

  private Object ensureFormGroup(Object page, String groupName) {
    Object group = MapUtil.builder().put("name", groupName).build();
    Object groupList = getProperty(page, "groupList");
    if (groupList == null) {
      addValue(page, "groupList", group);
      return group;
    }
    List l = (List) groupList;
    for (Object oneGroup : l) {
      Object name = getProperty(oneGroup, "name");
      if (groupName.equals(name)) {
        return oneGroup;
      }
    }
    addValue(page, "groupList", group);
    return group;
  }

  private void addFormActions(UserContext ctx, Object page, EntityDescriptor meta, Object data) {
    List<String> actions = getList(meta, "action", ListUtil.empty());
    actions.forEach(
        action -> {
          addValue(page, "actionList", createAction(ctx, data, action));
        });
  }

  public Map<Object, Object> createAction(UserContext ctx, Object data, String action) {
    String[] actionDes = action.split(":");
    String title, code;
    title = actionDes[0];
    if (actionDes.length > 1) {
      code = actionDes[1];
    } else {
      code = actionDes[0];
    }
    return MapUtil.builder()
        .put("title", title)
        .put("code", code)
        .put("linkToUrl", makeActionUrl(ctx, code, data))
        .build();
  }

  public Map<Object, Object> buildSelectAction(
      UserContext ctx, Object data, String action, String fieldName, Object id) {
    String[] actionDes = action.split(":");
    String title, code;
    title = actionDes[0];
    if (actionDes.length > 1) {
      code = actionDes[1];
    } else {
      code = actionDes[0];
    }
    return MapUtil.builder()
        .put("title", title)
        .put("code", code)
        .put("linkToUrl", makeGetActionUrl(ctx, code, data, MapUtil.of(fieldName, id)))
        .build();
  }

  public static void addValue(Object page, String key, Object value) {
    Object current = getProperty(page, key);
    if (current instanceof List) {
      ((List) current).add(value);
      return;
    }
    List l = new ArrayList();
    if (current != null) {
      l.add(current);
    }
    l.add(value);
    setValue(page, key, l);
  }

  public String makeActionUrl(UserContext ctx, String action, Object data) {
    if (data == null) {
      return String.format("%s/%s/", getBeanName(), action);
    } else {
      EntityDescriptor entity = ctx.resolveEntityDescriptor(((Entity) data).typeName());
      return makeActionUrl(ctx, entity, action);
    }
  }

  private String makeActionUrl(UserContext ctx, EntityDescriptor descriptor, String action) {
    if (descriptor == null) {
      return String.format("%s/%s/", getBeanName(), action);
    } else {
      ViewRender bean =
          ctx.getBean(ClassUtil.loadClass(descriptor.getTargetType().getName() + "Processor"));
      return String.format("%s/%s/", bean.getBeanName(), action);
    }
  }

  public String makeGetActionUrl(
      UserContext ctx, String action, Object data, Map<String, Object> additional) {
    if (data == null) {
      return String.format("%s/%s/", getBeanName(), action);
    } else {
      EntityDescriptor descriptor = ctx.resolveEntityDescriptor(((Entity) data).typeName());
      ViewRender bean =
          ctx.getBean(ClassUtil.loadClass(descriptor.getTargetType().getName() + "Processor"));
      return String.format(
          "%s/%s/%s/", bean.getBeanName(), action + "AsGet", encode(ctx, data, additional));
    }
  }

  public String encode(UserContext ctx, Object data, Map<String, Object> additional) {
    if (data == null) {
      errorMessage("data should not null");
    }
    EntityDescriptor entity = ctx.resolveEntityDescriptor(((Entity) data).typeName());
    Map<String, Object> values = new HashMap<>();
    for (PropertyDescriptor entry : entity.getProperties()) {
      String k = entry.getName();
      PropertyDescriptor v = entry;
      Object fieldValue = getFieldValue(data, k, v);
      if (fieldValue == null) {
        continue;
      }
      Object currentFieldValue = format(ctx, v, fieldValue);
      values.put(k, currentFieldValue);
    }
    values.putAll(additional);
    return Base64Encoder.encodeUrlSafe(JSONUtil.toJsonStr(values));
  }

  public void errorMessage(String message) {}

  private void addPageTitle(UserContext ctx, Object page, EntityDescriptor meta, Object data) {
    setValue(page, "pageTitle", getStr(meta, "name", "默认页面"));
  }

  private void addFormId(UserContext ctx, Object page, EntityDescriptor meta, Object data) {
    setValue(page, "id", data.getClass());
  }

  public static void setValue(Object page, String propertyPath, Object value) {
    BeanUtil.setProperty(page, propertyPath, value);
  }

  public void addFormClass(UserContext ctx, Object page, Object meta, Object data) {
    ctx.setResponseHeader(X_CLASS, "com.terapico.caf.viewcomponent.GenericFormPage");
  }

  public String getPageType(EntityDescriptor data) {
    return getStr(data, "page-type", FORM);
  }

  private String getStr(EntityDescriptor data, String key, String defaultValue) {
    return data.getStr(UI_ATTRIBUTE_PREFIX + key, defaultValue);
  }

  private Integer getInt(PropertyDescriptor data, String key, Integer defaultValue) {
    try {
      return Integer.parseInt(getStr(data, key, null));
    } catch (Exception e) {
      return defaultValue;
    }
  }

  public boolean getBoolean(PropertyDescriptor data, String key, boolean defaultValue) {
    return data.getBoolean(UI_ATTRIBUTE_PREFIX + key, defaultValue);
  }

  public List<String> getList(EntityDescriptor data, String key, List<String> defaultValue) {
    return data.getList(UI_ATTRIBUTE_PREFIX + key, defaultValue);
  }

  public static Object getProperty(Object value, String property) {
    if (value == null) {
      return null;
    }
    return BeanUtil.getProperty(value, property);
  }

  public void setFormAction(UserContext ctx, Object view, Object ret, String action) {
    setValue(view, "actionList", null);
    addValue(view, "actionList", createAction(ctx, ret, action));
  }

  public void addFormAction(UserContext ctx, Object view, Object ret, String action) {
    addValue(view, "actionList", createAction(ctx, ret, action));
  }

  public Object getAction(Object view, String action) {
    List actions = BeanUtil.getProperty(view, "actionList");
    if (ObjectUtil.isEmpty(actions)) {
      return null;
    }

    String[] actionDes = action.split(":");
    String code;
    if (actionDes.length > 1) {
      code = actionDes[1];
    } else {
      code = actionDes[0];
    }

    for (Object uiAction : actions) {
      if (code.equals(BeanUtil.getProperty(uiAction, "code"))) {
        return uiAction;
      }
    }
    return null;
  }

  public Object getField(Object view, String name) {
    List groups = BeanUtil.getProperty(view, "groupList");
    if (ObjectUtil.isEmpty(groups)) {
      return new HashMap<>();
    }
    for (Object group : groups) {
      List fieldList = BeanUtil.getProperty(group, "fieldList");
      for (Object field : fieldList) {
        if (name.equals(BeanUtil.getProperty(field, "name"))) {
          return field;
        }
      }
    }
    return new HashMap<>();
  }

  public void showFields(Object view, String... names) {
    if (names != null) {
      for (String name : names) {
        Object field = getField(view, name);
        BeanUtil.setProperty(field, "hidden", null);
      }
    }
  }

  public void hiddenFields(Object view, String... names) {
    if (names != null) {
      for (String name : names) {
        Object field = getField(view, name);
        BeanUtil.setProperty(field, "hidden", true);
      }
    }
  }

  public Object gotoNextView(BaseEntity currentView, Class<? extends BaseEntity> nextView) {
    if (ObjectUtil.isEmpty(currentView)) {
      return currentView;
    }
    if (ObjectUtil.isEmpty(nextView)) {
      return currentView;
    }
    Object o = ReflectUtil.newInstance(nextView);
    Object request = ReflectUtil.invoke(currentView, "getRequest");
    ReflectUtil.invoke(o, "setRequest", request);
    return o;
  }

  public <T extends Entity> T parseRequest(UserContext ctx, String request, Class<T> clazz) {
    if (ObjectUtil.isEmpty(request)) {
      return null;
    }
    Map<String, Object> input = JSONUtil.toBean(request, new TypeReference<>() {}, true);
    Set<String> keys = input.keySet();
    // set in context
    for (String key : keys) {
      if (key.startsWith(CTX)) {
        ctx.put(StrUtil.removePrefix(key, CTX), input.get(key));
      }
    }

    String req = (String) input.get("_req");
    if (!ObjectUtil.isEmpty(req)) {
      ctx.put("_req", req);
      String cachedObject = ctx.getInStore(req);
      if (cachedObject == null) {
        ctx.putInStore(req, req, 10);
      } else {
        ctx.duplicateFormException();
      }
    }

    T entity = ReflectUtil.newInstance(clazz);
    EntityDescriptor entityDescriptor = ctx.resolveEntityDescriptor(entity.typeName());
    while (entityDescriptor != null) {
      List<PropertyDescriptor> properties = entityDescriptor.getProperties();
      for (PropertyDescriptor property : properties) {
        boolean ignore = getBoolean(property, "ignore-form", false);
        if (ignore) {
          continue;
        }
        String name = property.getName();
        Object value = input.get(name);
        Class javaType = property.getType().javaType();
        if (value == null) {
          entity.setProperty(name, null);
        } else if (ClassUtil.isSimpleValueType(javaType)) {
          if (value instanceof Map || value instanceof Collection) {
            value = JSONUtil.toJsonStr(value);
          }
          entity.setProperty(name, Convert.convert(javaType, value));
        } else {
          // entity
          Number id;
          if (ClassUtil.isSimpleValueType(value.getClass())) {
            id = Convert.convert(Number.class, value);
          } else {
            id = BeanUtil.getProperty(value, "id");
          }
          if (ObjectUtil.isEmpty(id)) {
            entity.setProperty(name, null);
          } else {
            Entity v =
                ReflectUtil.invokeStatic(
                    ReflectUtil.getMethodByName(javaType, "refer"), id.longValue());
            entity.setProperty(name, v);
          }
        }
      }
      entityDescriptor = entityDescriptor.getParent();
    }
    return entity;
  }

  public void validate(UserContext ctx, Entity form) {
    try {
      ctx.checkAndFix(form);
    } catch (CheckException e) {
      List<CheckResult> violates = e.getViolates();
      List list = ctx.getList(NO_VALIDATE_FIELD);
      if (ObjectUtil.isEmpty(list)) {
        throw e;
      }
      List<CheckResult> realViolates = new ArrayList<>();
      for (CheckResult violate : violates) {
        ObjectLocation location = violate.getLocation();
        if (location instanceof HashLocation hashLocation) {
          String member = hashLocation.getMember();
          if (CollectionUtil.contains(list, member)) {
            continue;
          }
        }
        realViolates.add(violate);
      }
      if (ObjectUtil.isEmpty(realViolates)) {
        return;
      }
      throw new CheckException(realViolates);
    }
  }

  public void noValidateField(UserContext ctx, String... fields) {
    if (fields == null) {
      return;
    }
    for (String field : fields) {
      ctx.append(NO_VALIDATE_FIELD, field);
    }
  }
}
