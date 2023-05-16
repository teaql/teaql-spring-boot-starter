package io.teaql.data.event;

import io.teaql.data.BaseEntity;

public class EntityRecoverEvent {
  private BaseEntity item;

  //TODO: copy properties from item to local entity
  public EntityRecoverEvent(BaseEntity recoverItem) {
    this.item = recoverItem;
  }

  public BaseEntity getItem() {
    return item;
  }

  public void setItem(BaseEntity pItem) {
    item = pItem;
  }
}
