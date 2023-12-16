package io.teaql.data;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.extra.spring.SpringUtil;
import io.teaql.data.meta.EntityMetaFactory;
import io.teaql.data.meta.SimpleEntityMetaFactory;
import io.teaql.data.web.MultiReadFilter;
import io.teaql.data.web.ServletUserContextInitializer;
import io.teaql.data.web.UserContextInitializer;
import jakarta.servlet.Filter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.MethodParameter;
import org.springframework.core.annotation.Order;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;
import org.springframework.web.reactive.BindingContext;
import org.springframework.web.reactive.config.WebFluxConfigurer;
import org.springframework.web.reactive.result.method.annotation.ArgumentResolverConfigurer;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import reactor.core.publisher.Mono;

@Configuration
public class TQLAutoConfiguration {

  public static final String TQL_CONTEXT = "TQL_CONTEXT";

  @Bean
  @ConfigurationProperties(prefix = "teaql")
  public DataConfigProperties dataConfig() {
    return new DataConfigProperties();
  }

  @Bean
  @ConditionalOnMissingBean
  public EntityMetaFactory entityMetaFactory() {
    return new SimpleEntityMetaFactory();
  }

  @Bean
  public TQLResolver tqlResolver() {
    TQLResolver tqlResolver =
        new TQLResolver() {
          @Override
          public <T> T getBean(Class<T> clazz) {
            return SpringUtil.getBean(clazz);
          }

          @Override
          public <T> List<T> getBeans(Class<T> clazz) {
            Map<String, T> beansOfType = SpringUtil.getBeansOfType(clazz);
            if (ObjectUtil.isEmpty(beansOfType)) {
              return Collections.emptyList();
            }
            return new ArrayList<>(beansOfType.values());
          }

          @Override
          public <T> T getBean(String name) {
            return SpringUtil.getBean(name);
          }
        };
    GLobalResolver.registerResolver(tqlResolver);
    return tqlResolver;
  }

  @Bean
  @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
  public TQLContextResponseUpdater userContextResponseSetter() {
    return new TQLContextResponseUpdater();
  }

  @Configuration
  @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
  public static class TQLContextResolver implements HandlerMethodArgumentResolver {
    private DataConfigProperties config;

    public TQLContextResolver(@Autowired DataConfigProperties config) {
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
      Class<? extends UserContext> contextType = config.getContextClass();
      UserContext userContext = ReflectUtil.newInstanceIfPossible(contextType);
      userContext.init(webRequest.getNativeRequest());
      mavContainer.addAttribute(TQL_CONTEXT, userContext);
      return userContext;
    }

    @Bean
    public WebMvcConfigurer tqlConfigure(
        TQLContextResolver tqlResolver, TQLContextResponseUpdater userContextResponseSetter) {
      return new WebMvcConfigurer() {
        @Override
        public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
          resolvers.add(tqlResolver);
        }

        public void addInterceptors(InterceptorRegistry registry) {
          registry.addInterceptor(userContextResponseSetter);
        }
      };
    }
  }

  @Configuration
  @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.REACTIVE)
  public static class TQLReactiveContextResolver
      implements org.springframework.web.reactive.result.method.HandlerMethodArgumentResolver {
    private DataConfigProperties config;

    public TQLReactiveContextResolver(@Autowired DataConfigProperties config) {
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

    @Bean
    @ConditionalOnMissingBean
    public WebFluxConfigurer webFluxConfigurer(TQLReactiveContextResolver resolver) {
      return new WebFluxConfigurer() {
        @Override
        public void configureArgumentResolvers(ArgumentResolverConfigurer configurer) {
          configurer.addCustomResolver(resolver);
        }
      };
    }
  }

  @Bean("multiReadFilter")
  @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
  @Order
  public Filter multiReadRequest() {
    return new MultiReadFilter();
  }

  @Bean
  @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
  public UserContextInitializer servletInitializer() {
    return new ServletUserContextInitializer();
  }
}
