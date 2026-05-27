package io.teaql.data;

import io.teaql.data.utils.ReflectUtil;

public class DefaultUserContextFactory implements UserContextFactory {
    private final DataConfigProperties config;

    public DefaultUserContextFactory(DataConfigProperties config) {
        this.config = config;
    }

    @Override
    public UserContext create(Object request) {
        Class<? extends UserContext> contextType = config.getContextClass();
        UserContext userContext = ReflectUtil.newInstanceIfPossible(contextType);
        userContext.init(request);
        return userContext;
    }
}
