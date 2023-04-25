package io.teaql.data;

import cn.hutool.core.util.ReflectUtil;
import org.springframework.core.MethodParameter;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;

public class TQLContextResolver implements HandlerMethodArgumentResolver {
  private DataConfig config;

  public TQLContextResolver(DataConfig config) {
    this.config = config;
  }

  @Override
  public boolean supportsParameter(MethodParameter parameter) {
    return parameter.hasParameterAnnotation(TQLContext.class);
  }

  @Override
  public Object resolveArgument(
      MethodParameter parameter,
      ModelAndViewContainer mavContainer,
      NativeWebRequest webRequest,
      WebDataBinderFactory binderFactory)
      throws Exception {
    Class<? extends UserContext> contextType = config.contextType();
    UserContext userContext = ReflectUtil.newInstanceIfPossible(contextType);
    userContext.init(webRequest.getNativeRequest());
    return userContext;
  }
}
