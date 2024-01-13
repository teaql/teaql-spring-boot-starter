package io.teaql.graphql;

import cn.hutool.core.util.ClassUtil;
import cn.hutool.core.util.ObjUtil;
import io.teaql.data.BaseRequest;
import io.teaql.data.UserContext;
import java.lang.reflect.Method;
import java.util.List;

public abstract class BaseQueryContainer {
  protected abstract String type();

  public final void register(GraphqlQuerySupport factory) {
    if (factory == null) {
      return;
    }
    Class<? extends BaseQueryContainer> thisClass = this.getClass();
    List<Method> candidates =
        ClassUtil.getPublicMethods(
            thisClass,
            m -> {
              Class<?>[] parameterTypes = m.getParameterTypes();
              if (ObjUtil.isEmpty(parameterTypes)) {
                return false;
              }
              Class<?> parameterType = parameterTypes[0];
              if (!ClassUtil.isAssignable(UserContext.class, parameterType)) {
                return false;
              }

              Class<?> returnType = m.getReturnType();
              if (!ClassUtil.isAssignable(BaseRequest.class, returnType)) {
                return false;
              }
              return true;
            });
    for (Method candidate : candidates) {
      factory.register(
          new ReflectGraphqlFieldQuery(type() + ":" + candidate.getName(), this, candidate));
    }
  }
}
