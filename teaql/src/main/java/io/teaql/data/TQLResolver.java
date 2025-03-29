package io.teaql.data;

import java.util.List;

import cn.hutool.core.util.ObjectUtil;

import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.EntityMetaFactory;

public interface TQLResolver {
    default Repository resolveRepository(String type) {
        List<Repository> beans = getBeans(Repository.class);
        if (ObjectUtil.isNotEmpty(beans)) {
            for (Repository bean : beans) {
                EntityDescriptor entityDescriptor = bean.getEntityDescriptor();
                if (entityDescriptor.getType().equals(type)) {
                    return bean;
                }
            }
        }
        return null;
    }

    default EntityDescriptor resolveEntityDescriptor(String type) {
        EntityMetaFactory bean = getBean(EntityMetaFactory.class);
        if (bean != null) {
            return bean.resolveEntityDescriptor(type);
        }
        return null;
    }

    <T> T getBean(Class<T> clazz);

    <T> List<T> getBeans(Class<T> clazz);

    <T> T getBean(String name);
}
