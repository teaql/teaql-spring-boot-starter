package io.teaql.data.event;

import io.teaql.data.BaseEntity;

public class EntityDeletedEvent {
  private BaseEntity item;

  // TODO: copy properties from item to local entity
  public EntityDeletedEvent(BaseEntity item) {
    this.item = item;
  }

  public BaseEntity getItem() {
    return item;
  }

  public void setItem(BaseEntity pItem) {
    item = pItem;
  }
}
