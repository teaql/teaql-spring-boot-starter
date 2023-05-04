package io.teaql.data.event;

import io.teaql.data.BaseEntity;

public class EntityCreatedEvent {
  private BaseEntity item;

  public BaseEntity getItem() {
    return item;
  }

  public void setItem(BaseEntity pItem) {
    item = pItem;
  }
}
