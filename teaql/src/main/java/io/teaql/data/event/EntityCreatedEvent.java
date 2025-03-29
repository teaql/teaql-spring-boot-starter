package io.teaql.data.event;

import io.teaql.data.BaseEntity;

public class EntityCreatedEvent {
    private BaseEntity item;

    // TODO: copy properties from item to local entity
    public EntityCreatedEvent(BaseEntity item) {
        this.item = item;
    }

    public BaseEntity getItem() {
        return item;
    }

    public void setItem(BaseEntity pItem) {
        item = pItem;
    }
}
