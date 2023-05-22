package io.teaql.data.web;

import io.teaql.data.Entity;
import java.util.ArrayList;
import java.util.List;

public class WebAction {

  public static final String ACTION_LIST = "actionList";
  private String name;
  private String level;
  private String execute;
  private String target;
  private String component;
  private String warningMessage;

  public String getWarningMessage() {
    return warningMessage;
  }

  public void setWarningMessage(String warningMessage) {
    this.warningMessage = warningMessage;
  }

  public String getRoleForList() {
    return roleForList;
  }

  public void setRoleForList(String roleForList) {
    this.roleForList = roleForList;
  }

  private String roleForList;

  public String getComponent() {
    return component;
  }

  public void setComponent(String component) {
    this.component = component;
  }

  public String getRequestURL() {
    return requestURL;
  }

  public void setRequestURL(String requestURL) {
    this.requestURL = requestURL;
  }

  private String requestURL;

  public WebAction() {}

  public void bind(Entity entity) {
    if (entity != null) {
      entity.appendDynamicProperty(ACTION_LIST, this);
    }
  }

  public static WebAction viewWebAction() {
    WebAction webAction = new WebAction();
    webAction.setName("查看");
    webAction.setLevel("view");
    webAction.setExecute("switchview");
    webAction.setTarget("detail");
    return webAction;
  }

  public static WebAction viewSubListAction(String name, String listViewName, String roleForList) {
    WebAction webAction = new WebAction();
    webAction.setName(name);
    webAction.setLevel("view");
    webAction.setExecute("gotoList");
    webAction.setRoleForList(roleForList);
    webAction.setTarget(listViewName);
    return webAction;
  }

  public static WebAction simpleComponentAction(String name, String componentName) {
    WebAction webAction = new WebAction();
    webAction.setName(name);
    webAction.setComponent(componentName);
    return webAction;
  }

  public static WebAction modifyWebAction(String name, String url, String warningMessage) {
    WebAction webAction = new WebAction();
    webAction.setName(name);
    webAction.setLevel("modify");
    webAction.setExecute("switchview");
    webAction.setTarget("modify");
    webAction.setWarningMessage(warningMessage);
    webAction.setRequestURL(url);
    return webAction;
  }

  public static WebAction modifyWebAction(String name, String url) {
    return modifyWebAction(name, url, null);
  }

  public static WebAction modifyWebAction(String url) {
    return modifyWebAction("更新", url);
  }

  public static WebAction deleteWebAction(String url, String warningMessage) {

    return modifyWebAction("删除", url, warningMessage);
  }

  public static WebAction auditWebAction(String url, String warningMessage) {

    return modifyWebAction("审核", url, warningMessage);
  }

  public static WebAction discardWebAction(String url, String warningMessage) {

    return modifyWebAction("作废", url, warningMessage);
  }

  public static WebAction gotoAction(String name, String target, String url) {
    WebAction webAction = new WebAction();
    webAction.setName(name);
    webAction.setLevel("modify");
    webAction.setExecute("gotoview");
    webAction.setTarget(target);
    webAction.setRequestURL(url);
    return webAction;
  }

  public static WebAction switchViewAction(String viewName, String target) {
    WebAction webAction = new WebAction();
    webAction.setName(viewName);
    webAction.setLevel("modify");
    webAction.setExecute("switchview");
    webAction.setTarget(target);
    return webAction;
  }

  public static WebAction modifyWebAction() {
    WebAction webAction = new WebAction();
    webAction.setName("修改");
    webAction.setLevel("modify");
    webAction.setExecute("switchview");
    webAction.setTarget("modify");
    return webAction;
  }

  public static WebAction addNewWebAction(String odName) {
    WebAction webAction = new WebAction();
    webAction.setName("新增" + odName);
    webAction.setLevel("modify");
    webAction.setExecute("switchview");
    webAction.setTarget("addnew");
    return webAction;
  }

  public static WebAction deleteWebAction() {
    WebAction webAction = new WebAction();
    webAction.setName("删除");
    webAction.setLevel("delete");
    webAction.setExecute("switchview");
    webAction.setTarget("deleteview");
    return webAction;
  }

  public static List<WebAction> commonWebActions() {

    List<WebAction> webActions = new ArrayList<>();

    webActions.add(viewWebAction());
    webActions.add(modifyWebAction());

    return webActions;
  }

  public static WebAction batchUploadWebAction() {
    WebAction webAction = new WebAction();
    webAction.setName("批量上传");
    webAction.setLevel("modify");
    webAction.setExecute("switchview");
    webAction.setTarget("batchupload");
    return webAction;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getLevel() {
    return level;
  }

  public void setLevel(String level) {
    this.level = level;
  }

  public String getExecute() {
    return execute;
  }

  public void setExecute(String execute) {
    this.execute = execute;
  }

  public String getTarget() {
    return target;
  }

  public void setTarget(String target) {
    this.target = target;
  }
}
