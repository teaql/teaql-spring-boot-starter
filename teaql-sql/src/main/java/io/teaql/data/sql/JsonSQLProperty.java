package io.teaql.data.sql;

import java.nio.charset.StandardCharsets;
import java.sql.ResultSet;
import java.util.List;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import cn.hutool.core.codec.Base64;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.map.MapUtil;
import cn.hutool.core.util.ZipUtil;

import io.teaql.data.Entity;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;

public class JsonSQLProperty extends GenericSQLProperty implements SQLProperty {

    @Override
    public List<SQLData> toDBRaw(UserContext ctx, Entity entity, Object v) {
        ObjectMapper objectMapper = ctx.getBean(ObjectMapper.class);
        try {
            String value = objectMapper.writeValueAsString(v);
            Boolean zip = MapUtil.getBool(getAdditionalInfo(), "zip");
            if (zip != null && zip) {
                byte[] gzip = ZipUtil.gzip(value.getBytes(StandardCharsets.UTF_8));
                value = Base64.encode(gzip);
            }
            return super.toDBRaw(ctx, entity, value);
        }
        catch (JsonProcessingException pE) {
            throw new RepositoryException(pE);
        }
    }

    @Override
    public void setPropertyValue(UserContext ctx, Entity entity, ResultSet rs) {
        if (!findName(rs, getName())) {
            return;
        }
        ObjectMapper objectMapper = ctx.getBean(ObjectMapper.class);
        try {
            Class targetType = getType().javaType();
            Object value = getValue(rs);
            String jsonString = Convert.convert(String.class, value);
            Boolean zipped = MapUtil.getBool(getAdditionalInfo(), "zip");
            if (zipped != null && zipped) {
                byte[] decodeStr = Base64.decode(jsonString);
                byte[] bytes = ZipUtil.unGzip(decodeStr);
                jsonString = new String(bytes, StandardCharsets.UTF_8);
            }
            Object o = objectMapper.readValue(jsonString, targetType);
            entity.setProperty(getName(), o);
        }
        catch (JsonMappingException pE) {
            throw new RepositoryException(pE);
        }
        catch (JsonProcessingException pE) {
            throw new RepositoryException(pE);
        }
    }
}
