package io.teaql.data;

import cn.hutool.core.util.ReflectUtil;
import org.springframework.core.MethodParameter;
import org.springframework.web.reactive.BindingContext;
import org.springframework.web.reactive.result.method.HandlerMethodArgumentResolver;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

public class TQLReactiveContextResolver implements HandlerMethodArgumentResolver {
  private DataConfigProperties config;

  public TQLReactiveContextResolver(DataConfigProperties config) {
    this.config = config;
  }

  @Override
  public boolean supportsParameter(MethodParameter parameter) {
    return parameter.hasParameterAnnotation(TQLContext.class);
  }

  @Override
  public Mono<Object> resolveArgument(
      MethodParameter parameter, BindingContext bindingContext, ServerWebExchange exchange) {
    Class<? extends UserContext> contextType = config.getContextClass();
    UserContext userContext = ReflectUtil.newInstanceIfPossible(contextType);
    userContext.init(exchange);
    return Mono.just(userContext);
  }
}
