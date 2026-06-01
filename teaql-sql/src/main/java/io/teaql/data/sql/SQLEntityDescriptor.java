package io.teaql.data.sql;

import io.teaql.data.utils.BeanUtil;
import io.teaql.data.utils.NamingCase;
import io.teaql.data.meta.EntityDescriptor;

public class SQLEntityDescriptor extends EntityDescriptor {

    @Override
    protected GenericSQLProperty createPropertyDescriptor() {
        GenericSQLProperty p = new GenericSQLProperty();
        p.setTableName(NamingCase.toUnderlineCase(this.getType() + "_data"));
        return p;
    }

    @Override
    protected GenericSQLRelation createRelation() {
        GenericSQLRelation p = new GenericSQLRelation();
        p.setTableName(NamingCase.toUnderlineCase(this.getType() + "_data"));
        return p;
    }

    public void prepareSQLMeta(
            SQLProperty sqlProperty, String tableName, String columnName, String columnType) {
        BeanUtil.setProperty(sqlProperty, "tableName", tableName);
        BeanUtil.setProperty(sqlProperty, "columnName", columnName);
        BeanUtil.setProperty(sqlProperty, "columnType", columnType);
    }
}
