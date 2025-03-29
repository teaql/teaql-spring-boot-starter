package io.teaql.data.meta;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.util.BooleanUtil;
import cn.hutool.core.util.StrUtil;

/**
 * property meta in entity meta
 */
public class PropertyDescriptor {

    /**
     * property owner,
     */
    private EntityDescriptor owner;

    /**
     * property name
     */
    private String name;

    /**
     * property type
     */
    private PropertyType type;

    private Map<String, String> additionalInfo = new LinkedHashMap<>();

    public PropertyDescriptor() {
    }

    public PropertyDescriptor(String pPropertyName, PropertyType pType) {
        this.setName(pPropertyName);
        this.setType(pType);
    }

    public EntityDescriptor getOwner() {
        return owner;
    }

    public void setOwner(EntityDescriptor pOwner) {
        owner = pOwner;
    }

    public String getName() {
        return name;
    }

    public void setName(String pName) {
        name = pName;
    }

    public PropertyType getType() {
        return type;
    }

    public void setType(PropertyType pType) {
        type = pType;
    }

    public boolean isId() {
        return getName().equals("id");
    }

    public boolean isVersion() {
        return getName().equals("version");
    }

    public PropertyDescriptor with(String key, String value) {
        additionalInfo.put(key, value);
        return this;
    }

    public Map<String, String> getAdditionalInfo() {
        return additionalInfo;
    }

    public void setAdditionalInfo(Map<String, String> pAdditionalInfo) {
        additionalInfo = pAdditionalInfo;
    }

    public Map<String, String> getSelfAdditionalInfo() {
        return additionalInfo;
    }

    public boolean isIdentifier() {
        String identifier = getAdditionalInfo().get("identifier");
        return BooleanUtil.toBoolean(identifier);
    }

    public List<String> getCandidates() {
        String candidates = getAdditionalInfo().get("candidates");
        if (candidates == null) {
            return ListUtil.empty();
        }
        return StrUtil.split(candidates, ",", true, true);
    }

    public String getStr(String key, String value) {
        Map<String, String> additionalInfo = getAdditionalInfo();
        if (additionalInfo == null) {
            return value;
        }
        return additionalInfo.getOrDefault(key, value);
    }

    public boolean getBoolean(String key, boolean pDefaultValue) {
        String str = getStr(key, null);
        if (str == null) {
            return pDefaultValue;
        }
        return BooleanUtil.toBoolean(str);
    }
}
