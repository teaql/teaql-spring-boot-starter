package io.teaql.data.sql;

import cn.hutool.core.bean.BeanUtil;

import io.teaql.data.meta.EntityDescriptor;

public class SQLEntityDescriptor extends EntityDescriptor {

    @Override
    protected GenericSQLProperty createPropertyDescriptor() {
        return new GenericSQLProperty();
    }

    @Override
    protected GenericSQLRelation createRelation() {
        return new GenericSQLRelation();
    }

    public void prepareSQLMeta(
            SQLProperty sqlProperty, String tableName, String columnName, String columnType) {
        BeanUtil.setProperty(sqlProperty, "tableName", tableName);
        BeanUtil.setProperty(sqlProperty, "columnName", columnName);
        BeanUtil.setProperty(sqlProperty, "columnType", columnType);
    }
}
