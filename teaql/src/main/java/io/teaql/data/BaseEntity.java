package io.teaql.data;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import java.beans.PropertyChangeEvent;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class BaseEntity implements Entity {
  public static final String ID_PROPERTY = "id";
  public static final String VERSION_PROPERTY = "version";
  private Long id;
  private Long version;

  private EntityStatus $status = EntityStatus.NEW;

  @JsonIgnore
  private Map<String, PropertyChangeEvent> updatedProperties = new ConcurrentHashMap<>();

  @JsonIgnore private Map<String, Object> additionalInfo = new ConcurrentHashMap<>();

  @JsonIgnore private Map<String, Entity> relationCache = new HashMap<>();

  @JsonIgnore
  public EntityStatus get$status() {
    return $status;
  }

  public void set$status(EntityStatus p$status) {
    $status = p$status;
  }

  @Override
  public Long getId() {
    return id;
  }

  @Override
  public void setId(Long id) {
    this.id = id;
  }

  @Override
  public Long getVersion() {
    return version;
  }

  @Override
  public void setVersion(Long version) {
    this.version = version;
  }

  @Override
  public boolean newItem() {
    return $status == EntityStatus.NEW;
  }

  @Override
  public boolean updateItem() {
    return $status == EntityStatus.UPDATED;
  }

  @Override
  public boolean deleteItem() {
    return $status == EntityStatus.UPDATED_DELETED;
  }

  @Override
  public boolean needPersist() {
    return $status == EntityStatus.NEW
        || $status == EntityStatus.UPDATED
        || $status == EntityStatus.UPDATED_DELETED
        || $status == EntityStatus.UPDATED_RECOVER;
  }

  @Override
  public List<String> getUpdatedProperties() {
    return new ArrayList<>(updatedProperties.keySet());
  }

  @Override
  public void addRelation(String relationName, Entity value) {
    Field field = ReflectUtil.getField(this.getClass(), relationName);
    Class<?> type = field.getType();
    if (SmartList.class.isAssignableFrom(type)) {
      SmartList existing = (SmartList) getProperty(relationName);
      if (existing == null) {
        existing = new SmartList();
        setProperty(relationName, existing);
      }
      existing.add(value);
    } else if (Entity.class.isAssignableFrom(type)) {
      setProperty(relationName, value);
    }
  }

  @Override
  public void addDynamicProperty(String propertyName, Object value) {
    this.additionalInfo.put(dynamicPropertyNameOf(propertyName), value);
  }

  public String dynamicPropertyNameOf(String propertyName) {
    if (propertyName.startsWith(".") && propertyName.length() > 1) {
      return propertyName.substring(1);
    }
    return String.join("", "_", propertyName);
  }

  @Override
  public void appendDynamicProperty(String propertyName, Object value) {
    propertyName = dynamicPropertyNameOf(propertyName);
    List list = (List) this.additionalInfo.get(propertyName);
    if (list == null) {
      list = new ArrayList<>();
      this.additionalInfo.put(propertyName, list);
    }
    list.add(value);
  }

  @Override
  public <T> T getDynamicProperty(String propertyName) {
    return getDynamicProperty(propertyName, null);
  }

  @Override
  public void markAsDeleted() {
    gotoNextStatus(EntityAction.DELETE);
  }

  @Override
  public void markAsRecover() {
    gotoNextStatus(EntityAction.RECOVER);
  }

  public <T> T getDynamicProperty(String propertyName, T defaultValue) {
    Object o = this.additionalInfo.get(dynamicPropertyNameOf(propertyName));
    if (o == null) {
      return defaultValue;
    }
    return (T) o;
  }

  @JsonAnyGetter
  public Map<String, Object> getAdditionalInfo() {
    return additionalInfo;
  }

  @JsonAnySetter
  public void setAdditionalInfo(Map<String, Object> pAdditionalInfo) {
    additionalInfo = pAdditionalInfo;
  }

  @Override
  public boolean equals(Object pO) {
    if (this == pO) return true;
    if (pO == null || getClass() != pO.getClass()) return false;
    BaseEntity that = (BaseEntity) pO;
    return Objects.equals(getId(), that.getId())
        && Objects.equals(getVersion(), that.getVersion())
        && Objects.equals(typeName(), that.typeName());
  }

  @Override
  public int hashCode() {
    return Objects.hash(getId(), getVersion(), typeName());
  }

  @Override
  public Object getProperty(String propertyName) {
    Entity o = this.relationCache.get(propertyName);
    if (o != null) {
      return o;
    }
    return Entity.super.getProperty(propertyName);
  }

  /**
   * callbacks for updateXXX methods
   *
   * @param propertyName
   * @param oldValue
   * @param newValue
   */
  public void handleUpdate(String propertyName, Object oldValue, Object newValue) {
    gotoNextStatus(EntityAction.UPDATE);
    PropertyChangeEvent propertyChangeEvent = updatedProperties.get(propertyName);
    // 多次变化记录最开始的值为old值
    if (propertyChangeEvent != null) {
      oldValue = propertyChangeEvent.getOldValue();
    }
    // 值在多次变化后，实际没有变化
    if (ObjectUtil.equals(oldValue, newValue)) {
      updatedProperties.remove(propertyName);
      return;
    }
    updatedProperties.put(
        propertyName, new PropertyChangeEvent(this, propertyName, oldValue, newValue));
  }

  public void gotoNextStatus(EntityAction action) {
    set$status(get$status().next(action));
  }

  public void cacheRelation(String relationName, Entity relation) {
    this.relationCache.put(relationName, relation);
    Object initValue = Entity.super.getProperty(relationName);
    handleUpdate(relationName, initValue, relation);
  }

  public Object getOldValue(String propertyName) {
    PropertyChangeEvent propertyChangeEvent = updatedProperties.get(propertyName);
    if (propertyChangeEvent == null) {
      return null;
    }
    return propertyChangeEvent.getOldValue();
  }

  public Object getNewValue(String propertyName) {
    PropertyChangeEvent propertyChangeEvent = updatedProperties.get(propertyName);
    if (propertyChangeEvent == null) {
      return null;
    }
    return propertyChangeEvent.getNewValue();
  }

  public BaseEntity markToRemove() {
    gotoNextStatus(EntityAction.DELETE);
    return this;
  }

  public BaseEntity markToRecover() {
    gotoNextStatus(EntityAction.RECOVER);
    return this;
  }

  @Override
  public void delete(UserContext userContext) {
    markToRemove();
    userContext.saveGraph(this);
  }

  @Override
  public BaseEntity recover(UserContext userContext) {
    markToRecover();
    userContext.saveGraph(this);
    return this;
  }

  @Override
  public boolean recoverItem() {
    return $status == EntityStatus.UPDATED_RECOVER;
  }

  public void clearUpdatedProperties() {
    this.updatedProperties.clear();
  }
}
