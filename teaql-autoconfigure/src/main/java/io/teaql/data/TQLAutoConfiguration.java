package io.teaql.data;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.extra.spring.SpringUtil;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.teaql.data.jackson.TeaQLModule;
import io.teaql.data.meta.EntityMetaFactory;
import io.teaql.data.meta.SimpleEntityMetaFactory;
import io.teaql.data.redis.RedisStore;
import io.teaql.data.translation.Translator;
import io.teaql.data.web.MultiReadFilter;
import io.teaql.data.web.ServletUserContextInitializer;
import io.teaql.data.web.UITemplateRender;
import io.teaql.data.web.UserContextInitializer;
import jakarta.servlet.Filter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.autoconfigure.jackson.Jackson2ObjectMapperBuilderCustomizer;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.MethodParameter;
import org.springframework.core.annotation.Order;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.http.MediaType;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;
import org.springframework.web.reactive.BindingContext;
import org.springframework.web.reactive.config.WebFluxConfigurer;
import org.springframework.web.reactive.result.method.annotation.ArgumentResolverConfigurer;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;
import reactor.core.publisher.Mono;

@Configuration
public class TQLAutoConfiguration {
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
  @ConditionalOnMissingBean
  public Translator translator() {
    return Translator.NOOP;
  }

  @Bean("redisTemplate")
  public RedisTemplate<String, Object> redisTemplate(
      RedisConnectionFactory redisConnectionFactory) {
    RedisTemplate<String, Object> template = new RedisTemplate<>();
    template.setDefaultSerializer(RedisSerializer.json());
    template.setConnectionFactory(redisConnectionFactory);
    return template;
  }

  @Bean
  @ConditionalOnMissingBean(name = "templateRender")
  public UITemplateRender templateRender() {
    return new UITemplateRender();
  }

  @Bean
  public DataStore dataStore(RedisTemplate<String, Object> redisTemplate) {
    return new RedisStore(redisTemplate);
  }

  @Bean
  @ConditionalOnProperty(
      prefix = "teaql",
      name = "useSmartListAsList",
      havingValue = "true",
      matchIfMissing = true)
  public Jackson2ObjectMapperBuilderCustomizer smartListSerializer() {
    return jacksonObjectMapperBuilder -> {
      jacksonObjectMapperBuilder.postConfigurer(
          mapper -> {
            mapper.registerModule(TeaQLModule.INSTANCE);
            mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
          });
    };
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
      userContext.init(webRequest);
      return userContext;
    }

    @Bean
    public WebMvcConfigurer tqlConfigure(TQLContextResolver tqlResolver) {
      return new WebMvcConfigurer() {
        @Override
        public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
          resolvers.add(tqlResolver);
        }
      };
    }
  }

  @ControllerAdvice
  @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
  public static class XClassSetter implements ResponseBodyAdvice {
    @Override
    public boolean supports(MethodParameter returnType, Class converterType) {
      return true;
    }

    @Override
    public Object beforeBodyWrite(
        Object body,
        MethodParameter returnType,
        MediaType selectedContentType,
        Class selectedConverterType,
        ServerHttpRequest request,
        ServerHttpResponse response) {
      String xClass = response.getHeaders().getFirst(UserContext.X_CLASS);
      if (ObjectUtil.isEmpty(xClass) && body != null) {
        response.getHeaders().set(UserContext.X_CLASS, body.getClass().getName());
      }

      return body;
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
