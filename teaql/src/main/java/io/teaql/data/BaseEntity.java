package io.teaql.data;

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
        || $status == EntityStatus.UPDATED_DELETED;
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
    this.additionalInfo.put(propertyName, value);
  }

  @Override
  public void appendDynamicProperty(String propertyName, Object value) {
    List list = (List) this.additionalInfo.get(propertyName);
    if (list == null) {
      list = new ArrayList<>();
      this.additionalInfo.put(propertyName, list);
    }
    list.add(value);
  }

  @Override
  public Object getDynamicProperty(String propertyName) {
    return this.additionalInfo.get(propertyName);
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

  public void cacheRelation(String relationName, Entity relation) {
    this.relationCache.put(relationName, relation);
  }
}
