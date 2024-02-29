package io.teaql.data.web;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.io.resource.ResourceUtil;
import cn.hutool.core.map.MapUtil;
import cn.hutool.core.util.BooleanUtil;
import cn.hutool.core.util.NumberUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONConfig;
import cn.hutool.json.JSONUtil;
import io.teaql.data.BaseEntity;
import io.teaql.data.Entity;
import io.teaql.data.UserContext;
import io.teaql.data.meta.PropertyDescriptor;
import java.util.*;

public class UITemplateRender {
  public static String messageTemplate =
      ResourceUtil.readUtf8Str("classpath:io/teaql/data/web/message.json");

  public static String kvTemplate = ResourceUtil.readUtf8Str("classpath:io/teaql/data/web/kv.json");

  public static String tableTemplate =
      ResourceUtil.readUtf8Str("classpath:io/teaql/data/web/table.json");

  public static String imagesTemplate =
      ResourceUtil.readUtf8Str("classpath:io/teaql/data/web/images.json");

  JSONConfig config = JSONConfig.create().setIgnoreNullValue(true);

  public void kv(
      UserContext ctx,
      PropertyDescriptor meta,
      Object uiField,
      Object fieldValue,
      List candidates,
      List mappedCandidates) {
    Map value = JSONUtil.toBean(kvTemplate, config, Map.class);
    Object kids0 = ViewRender.getProperty(value, "kids[0]");

    setTemplatePassThrough(meta, kids0);
    ViewRender.setValue(kids0, "items", null);
    int index = 1;
    for (Object o : mappedCandidates) {
      Object id = ViewRender.getProperty(o, "id");
      ViewRender.addValue(
          kids0,
          "items",
          MapUtil.builder()
              .put("id", id == null ? index++ : id)
              .put("title", ViewRender.getProperty(o, "title"))
              .put("value", ViewRender.getProperty(o, "value"))
              .build());
    }

    if (!meta.getBoolean("ui_no_label", false)) {
      ViewRender.setValue(kids0, "title", meta.getStr("zh_CN", null));
    }
    ViewRender.setValue(uiField, "value", value);
  }

  private void setTemplatePassThrough(PropertyDescriptor meta, Object uiElement) {
    meta.getAdditionalInfo()
        .forEach(
            (k, value) -> {
              String key = k;
              if (!key.startsWith("ui_$template_")) {
                return;
              }
              String property = StrUtil.removePrefix(key, "ui_$template_");

              if (StrUtil.endWith(property, "_number")) {
                property = StrUtil.removeSuffix(property, "_number");
                ViewRender.setValue(uiElement, property, NumberUtil.parseNumber((String) value));
                return;
              }

              if (StrUtil.endWith(property, "_bool")) {
                property = StrUtil.removeSuffix(property, "_bool");
                ViewRender.setValue(uiElement, property, BooleanUtil.toBoolean((String) value));
                return;
              }
              ViewRender.setValue(uiElement, property, value);
            });
  }

  public void message(UserContext ctx, PropertyDescriptor meta, Object uiField, String message) {
    Map value = JSONUtil.toBean(messageTemplate, config, Map.class);
    BeanUtil.setProperty(value, "kids[0].text", message);
    BeanUtil.setProperty(uiField, "value", value);
  }

  public void error(UserContext ctx, PropertyDescriptor meta, Object uiField, String message) {
    Map value = JSONUtil.toBean(messageTemplate, config, Map.class);
    BeanUtil.setProperty(value, "kids[0].text", message);
    BeanUtil.setProperty(value, "kids[0].style.color", "#f23030"); // rea
    BeanUtil.setProperty(uiField, "value", value);
  }

  public void json(UserContext ctx, PropertyDescriptor meta, Object uiField, Object obj) {
    if (obj == null) {
      ViewRender.setValue(uiField, "value", null);
      return;
    }
    ViewRender.setValue(uiField, "value", JSONUtil.toBean(String.valueOf(obj), config, Map.class));
  }

  public void jsonArray(UserContext ctx, PropertyDescriptor meta, Object uiField, Object obj) {
    if (obj == null) {
      ViewRender.setValue(uiField, "value", null);
      return;
    }
    ViewRender.setValue(uiField, "value", JSONUtil.toBean(String.valueOf(obj), config, List.class));
  }

  public void stringArray(UserContext ctx, PropertyDescriptor meta, Object uiField, Object obj) {
    if (obj == null) {
      ViewRender.setValue(uiField, "value", null);
      return;
    }
    ViewRender.setValue(uiField, "value", JSONUtil.toList(String.valueOf(obj), String.class));
  }

  public void images(UserContext ctx, PropertyDescriptor meta, Object uiField, Object obj) {
    if (obj == null) {
      BeanUtil.setProperty(uiField, "hidden", "true");
      return;
    }
    Map value = JSONUtil.toBean(imagesTemplate, config, Map.class);
    BeanUtil.setProperty(
        value, "kids[0].items", JSONUtil.toBean(String.valueOf(obj), config, List.class));
    ViewRender.setValue(uiField, "value", value);
  }

  public void table(
      UserContext ctx,
      PropertyDescriptor meta,
      Object uiField,
      Object fieldValue,
      List candidates,
      List mappedCandidates) {
    Map value = JSONUtil.toBean(tableTemplate, config, Map.class);
    Object kids0 = ViewRender.getProperty(value, "kids[0]");

    // title
    ViewRender.setValue(kids0, "title", meta.getStr("zh_CN", null));
    ViewRender.setValue(uiField, "value", value);

    // clear data
    ViewRender.setValue(kids0, "data", null);

    Object first = CollectionUtil.getFirst(candidates);

    //
    Object backgroundColor = BeanUtil.getProperty(first, "backgroundColor");
    boolean firstIsColor = backgroundColor != null;
    if (firstIsColor) {
      BeanUtil.setProperty(kids0, "backgroundColor", backgroundColor);
      candidates = candidates.subList(1, candidates.size());
    }

    if (ObjectUtil.isEmpty(candidates)) {
      return;
    }

    Object header = CollectionUtil.getFirst(candidates);
    if (ObjectUtil.isEmpty(header)) {
      return;
    }
    Map<String, Object> headerMap = BeanUtil.toBean(header, LinkedHashMap.class);
    Set<String> columns = headerMap.keySet();

    // add header
    Map<String, Object> headerRow = new HashMap<>();
    headerRow.put("header", true);
    ViewRender.addValue(kids0, "data", headerRow);
    // add header columns
    for (String column : columns) {
      ViewRender.addValue(
          headerRow,
          "items",
          MapUtil.builder().put("title", column).put("colspan", headerMap.get(column)).build());
    }

    // add data rows
    List dataRows = candidates.subList(1, candidates.size());
    if (ObjectUtil.isEmpty(dataRows)) {
      return;
    }

    for (Object dataRow : dataRows) {
      Map<String, Object> row = new HashMap<>();
      ViewRender.addValue(kids0, "data", row);

      Map<String, Object> map = BeanUtil.toBean(dataRow, Map.class);
      Set<String> keys = map.keySet();
      keys.removeAll(columns);

      for (String column : columns) {
        ViewRender.addValue(
            row,
            "items",
            MapUtil.builder()
                .put("title", BeanUtil.getProperty(dataRow, column))
                .put("colspan", headerMap.get(column))
                .build());
      }

      for (String key : keys) {
        ViewRender.setValue(row, key, map.get(key));
      }
    }
  }

  public Object showPop(UserContext ctx, BaseEntity data) throws Exception {
    return ServiceRequestUtil.showPop(ctx, data);
  }

  public void createStandardConfirmPopup(
      UserContext ctx,
      Entity request,
      String title,
      String text,
      String cancelActionUrl,
      String confirmActionUrl,
      String playSound) {
    Map<String, Object> popup = new HashMap<>();
    BeanUtil.setProperty(popup, "popup.title", title);
    BeanUtil.setProperty(popup, "popup.text", text);

    Map<Object, Object> confirmAction =
        MapUtil.builder()
            .put("title", "CONFIRM")
            .put("code", "confirm")
            .put("linkToUrl", confirmActionUrl)
            .build();
    if (!ObjectUtil.isEmpty(cancelActionUrl)) {
      Map<Object, Object> cancelAction =
          MapUtil.builder()
              .put("title", "CANCEL")
              .put("code", "cancel")
              .put("linkToUrl", cancelActionUrl)
              .build();
      BeanUtil.setProperty(popup, "popup.actionList", ListUtil.of(cancelAction, confirmAction));
    } else {
      BeanUtil.setProperty(popup, "popup.actionList", ListUtil.of(confirmAction));
    }
    BeanUtil.setProperty(popup, "playSound", playSound);
    ctx.putInStore(serviceRequestPopupKey(request), popup, 10);
  }

  static String serviceRequestPopupKey(Entity request) {
    return StrUtil.format("serviceRequest:popup:{}", request.getId());
  }

  public void createConfirmOnlyPopup(
      UserContext ctx,
      Entity request,
      String title,
      String text,
      String confirmAction,
      String playSound) {
    createStandardConfirmPopup(ctx, request, title, text, null, confirmAction, playSound);
  }
}
