package io.teaql.data;

import java.lang.reflect.Method;
import java.util.List;

import io.teaql.data.utils.BeanUtil;
import io.teaql.data.utils.ReflectUtil;
import io.teaql.data.utils.StrUtil;

// the super interface in TEAQL repository
public interface Entity {
    Long getId();

    void setId(Long id);

    Long getVersion();

    void setVersion(Long id);

    default String typeName() {
        return this.getClass().getSimpleName();
    }

    default String runtimeType() {
        return typeName();
    }

    ;

    default void setRuntimeType(String runtimeType) {
    }

    default Entity save(UserContext userContext) {
        userContext.checkAndFix(this);
        userContext.saveGraph(this);
        return this;
    }

    default void delete(UserContext userContext) {
    }

    default Entity recover(UserContext userContext) {
        return this;
    }

    boolean newItem();

    boolean updateItem();

    boolean deleteItem();

    default boolean recoverItem() {
        return false;
    }

    boolean needPersist();

    default <T> T getProperty(String propertyName) {
        return BeanUtil.getProperty(this, propertyName);
    }

    default void setProperty(String propertyName, Object value) {
        BeanUtil.setProperty(this, propertyName, value);
    }

    default Entity updateProperty(String propertyName, Object value) {
        Method method =
                ReflectUtil.getMethodByName(getClass(), "update" + StrUtil.upperFirst(propertyName));
        ReflectUtil.invoke(this, method, value);
        return this;
    }

    List<String> getUpdatedProperties();

    void addRelation(String relationName, Entity value);

    void addDynamicProperty(String propertyName, Object value);

    void appendDynamicProperty(String propertyName, Object value);

    <T> T getDynamicProperty(String propertyName);

    void markAsDeleted();

    void markAsRecover();

    default String getComment() {
        return null;
    }

    default void setComment(String comment) {
    }

    default String getTraceChain() {
        return null;
    }

    default void setTraceChain(String traceChain) {
    }
}
