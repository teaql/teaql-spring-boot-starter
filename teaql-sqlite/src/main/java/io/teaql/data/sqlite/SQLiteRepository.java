package io.teaql.data.sqlite;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.sql.DataSource;

import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.map.CaseInsensitiveMap;
import cn.hutool.core.text.NamingCase;
import cn.hutool.core.util.StrUtil;

import io.teaql.data.BaseEntity;
import io.teaql.data.Entity;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;
import io.teaql.data.meta.SimplePropertyType;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLRepository;




public class SQLiteRepository<T extends Entity> extends SQLRepository<T> {
    public SQLiteRepository(EntityDescriptor entityDescriptor, DataSource dataSource) {
        super(entityDescriptor, dataSource);
        registerExpressionParser(SQLiteAggrExpressionParser.class);
        registerExpressionParser(SQLiteParameterParser.class);
        registerExpressionParser(SQLiteTwoOperatorExpressionParser.class);
    }
    //name

    protected String getSchemaColumnNameFieldName() {
        return "name";
    }

    public static class ParsedType {
        public String dataType;
        public Integer length; // 可以为 null

        @Override
        public String toString() {
            return "dataType='" + dataType + '\'' + ", length=" + length;
        }
    }

    public static ParsedType parseType(String type) {
        ParsedType result = new ParsedType();

        // 正则匹配：例如 VARCHAR(100)
        Pattern pattern = Pattern.compile("([a-zA-Z]+)\\s*\\(?\\s*(\\d*)\\s*\\)?");
        Matcher matcher = pattern.matcher(type.trim());

        if (matcher.find()) {
            result.dataType = matcher.group(1);
            String len = matcher.group(2);
            if (len != null && !len.isEmpty()) {
                result.length = Integer.parseInt(len);
            } else {
                result.length = null;
            }
        } else {
            result.dataType = type.trim();
            result.length = null;
        }

        return result;
    }

    protected boolean isTypeMatch(String dbType, String type) {
        if (dbType.equalsIgnoreCase(type)) {
            return true;
        }
        return dbType.equalsIgnoreCase("real") && type.toLowerCase().startsWith("numeric(");
    }
    protected void alterColumn(UserContext ctx, List<Map<String, Object>> tableInfo, String table, List<SQLColumn> columns, SQLColumn column) {

        String backupTableName=String.format("%s_000000",table);
        //backup table
        super.executeUpdate(ctx, String.format("ALTER TABLE %s RENAME TO %s",table,backupTableName));
        //recreate
        super.createTable(ctx,table,columns);
        //import
        super.executeUpdate(ctx,String.format("INSERT INTO %s SELECT * FROM %s",table,backupTableName));
        //drop backup
        super.executeUpdate(ctx,String.format("DROP TABLE %s",backupTableName));


        //ctx.info("trying to alter {}" , column);
    }

    protected String calculateDBType(Map<String, Object> columnInfo) {
        String dataType = (String) columnInfo.get("type");
        ParsedType result = parseType(dataType.toLowerCase());
        String lowercaseType = result.dataType;
        switch (lowercaseType) {
            case "bigint":
                return "bigint";
            case "tinyint":
            case "boolean":
                return "boolean";
            case "varchar":
            case "character varying":
                return StrUtil.format("varchar({})", result.length);
            case "date":
                return "date";
            case "int":
            case "integer":
                return "integer";
            case "decimal":
            case "numeric":
                return "real";
            case "text":
                return "text";
            case "time without time zone":
                return "time";
            case "timestamp":
            case "timestamp without time zone":
                return "timestamp";
            default:
                throw new RepositoryException("unsupported type:" + lowercaseType);
        }
    }
    @Override
    protected String getSQLForUpdateWhenPrepareId() {
        return "SELECT current_level from {} WHERE type_name = '{}'";
    }

    @Override
    protected void ensureIndexAndForeignKey(UserContext ctx) {

        this.getEntityDescriptor().getOwnProperties().forEach(propertyDescriptor -> {

            if(needToCreateIndex(ctx,propertyDescriptor)){
                ensureIndex(ctx,propertyDescriptor);
            }
        });


    }

    private Set<String> indexNames;



    protected boolean needToCreateIndex(UserContext ctx,PropertyDescriptor propertyDescriptor){

        if(indexNames==null){
            indexNames=new HashSet<>();
            List<Map<String, Object>> mapList = queryForList("select name from sqlite_master  where type='index'", Collections.emptyMap());
            mapList.forEach(entry->{
                indexNames.add(entry.get("name").toString());
            });

        }
        return !indexNames.contains(indexName(propertyDescriptor));
    }


    protected void ensureIndex(UserContext ctx, PropertyDescriptor propertyDescriptor) {
        if(propertyDescriptor.isId()){
            return;
        }

        if(!isToCreateIndexFor(propertyDescriptor)){
            return;
        }

        String statement=StrUtil.format("CREATE INDEX IF NOT EXISTS {} ON {}({})",
                indexName(propertyDescriptor),
                tableName(getEntityDescriptor().getType()),
                columnName(propertyDescriptor.getName())
        );

        this.executeUpdate(ctx,statement);
    }

    protected boolean isToCreateIndexFor(PropertyDescriptor propertyDescriptor) {

        if(propertyDescriptor.isId()){
            return false;
        }
        if(propertyDescriptor.isIdentifier()){
            return true;
        }
        if(propertyDescriptor.getType() instanceof SimplePropertyType type){

            if(Date.class.isAssignableFrom(type.javaType())){
                return true;
            }
            if(BaseEntity.class.isAssignableFrom(type.javaType())){
                return true;
            }
            if(Number.class.isAssignableFrom(type.javaType())){
                return true;
            }
            if(Timestamp.class.isAssignableFrom(type.javaType())){
                return true;
            }
            if(LocalDateTime.class.isAssignableFrom(type.javaType())){
                return true;
            }
            return false;
        }



        return  false;



    }

    protected String indexName(PropertyDescriptor propertyDescriptor){
        return StrUtil.format("idx_{}_of_{}",NamingCase.toUnderlineCase(propertyDescriptor.getName()),NamingCase.toUnderlineCase(getEntityDescriptor().getType()));
    }
    protected String columnName(String propertyName){
        return NamingCase.toUnderlineCase(propertyName);
    }
    @Override
    protected void ensure(
            UserContext ctx, List<Map<String, Object>> tableInfo, String table, List<SQLColumn> columns) {
        tableInfo = CollStreamUtil.toList(tableInfo, CaseInsensitiveMap::new);
        super.ensure(ctx, tableInfo, table, columns);
    }

    // protected String getPureColumnName(String columnName) {
    //     return StrUtil.unWrap(columnName, '`');
    // }
    /*SELECT
    cid,
    name,
    type AS data_type,  -- 这里添加别名
    notnull,
    dflt_value,
    pk
FROM PRAGMA_table_info('表名')*/
    @Override
    protected String findTableColumnsSql(DataSource dataSource, String table) {
        try (Connection connection = dataSource.getConnection()) {
            //String databaseName = connection.getCatalog();
            return String.format(
                    //"SELECT name FROM sqlite_master WHERE type='table' AND name='%s'",
                    "PRAGMA table_info(%s)",
                    table);
        }
        catch (SQLException pE) {
            throw new RuntimeException(pE);
        }
    }
}
