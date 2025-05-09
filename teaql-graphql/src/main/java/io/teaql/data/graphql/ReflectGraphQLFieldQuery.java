package io.teaql.data.graphql;

import java.lang.reflect.Method;

import cn.hutool.core.util.ReflectUtil;

import io.teaql.data.BaseRequest;
import io.teaql.data.UserContext;

public class ReflectGraphQLFieldQuery implements GraphQLFieldQuery {

    private final String id;
    private final Object obj;
    private final Method method;

    public ReflectGraphQLFieldQuery(String id, Object obj, Method method) {
        this.id = id;
        this.obj = obj;
        this.method = method;
    }

    @Override
    public String id() {
        return id;
    }

    @Override
    public BaseRequest buildQuery(UserContext userContext, Object[] parameters) {
        Object[] invokeParameters = new Object[parameters.length + 1];
        invokeParameters[0] = userContext;
        System.arraycopy(parameters, 0, invokeParameters, 1, parameters.length);
        return ReflectUtil.invoke(obj, method, invokeParameters);
    }

    @Override
    public String getRequestProperty() {
        QueryProperty annotation = method.getAnnotation(QueryProperty.class);
        if (annotation != null) {
            return annotation.value();
        }
        return method.getName();
    }
}
