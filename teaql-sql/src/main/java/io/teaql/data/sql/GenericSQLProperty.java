package io.teaql.data.sql;

import java.sql.ResultSet;
import java.util.List;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.util.ReflectUtil;

import io.teaql.data.BaseEntity;
import io.teaql.data.Entity;
import io.teaql.data.EntityStatus;
import io.teaql.data.UserContext;
import io.teaql.data.meta.PropertyDescriptor;

public class GenericSQLProperty extends PropertyDescriptor implements SQLProperty {
    private String tableName;
    private String columnName;
    private String columnType;

    public GenericSQLProperty(String pTableName, String pColumnName, String pType) {
        tableName = pTableName;
        columnName = pColumnName;
        columnType = pType;
    }

    public GenericSQLProperty() {
    }

    @Override
    public List<SQLColumn> columns() {
        SQLColumn sqlColumn = new SQLColumn(tableName, columnName);
        sqlColumn.setType(columnType);
        return ListUtil.of(sqlColumn);
    }

    @Override
    public List<SQLData> toDBRaw(UserContext ctx, Entity entity, Object value) {
        SQLData d = new SQLData();
        d.setColumnName(columnName);
        d.setTableName(tableName);
        if (value instanceof Entity) {
            d.setValue(((Entity) value).getId());
        }
        else {
            d.setValue(value);
        }
        return ListUtil.of(d);
    }

    @Override
    public void setPropertyValue(UserContext ctx, Entity entity, ResultSet rs) {
        if (!findName(rs, getName())) {
            return;
        }
        Class targetType = getType().javaType();
        if (Entity.class.isAssignableFrom(targetType)) {
            Entity o = createRefer(rs);
            entity.setProperty(getName(), o);
        }
        else {
            Object value = getValue(rs);
            entity.setProperty(getName(), Convert.convert(targetType, value));
        }
    }

    protected Object getValue(ResultSet rs) {
        return ResultSetTool.getValue(rs, getName());
    }

    protected boolean findName(ResultSet resultSet, String name) {
        try {
            int columnCount = resultSet.getMetaData().getColumnCount();
            for (int i = 0; i < columnCount; i++) {
                String columnLabel = resultSet.getMetaData().getColumnLabel(i + 1);
                if (columnLabel.equalsIgnoreCase(name)) {
                    return true;
                }
            }
        }
        catch (Exception e) {

        }
        return false;
    }

    private Entity createRefer(ResultSet rs) {
        BaseEntity o = (BaseEntity) ReflectUtil.newInstance(getType().javaType());
        Object referId = getValue(rs);

        if (referId == null) {
            return null;
        }
        o.setId(((Number) referId).longValue());
        o.set$status(EntityStatus.REFER);
        return o;
    }

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String pTableName) {
        tableName = pTableName;
    }

    public String getColumnName() {
        return columnName;
    }

    public void setColumnName(String pColumnName) {
        columnName = pColumnName;
    }

    public String getColumnType() {
        return columnType;
    }

    public void setColumnType(String pColumnType) {
        columnType = pColumnType;
    }
}
