package io.teaql.data;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.extra.spring.SpringUtil;
import com.zaxxer.hikari.HikariDataSource;
import io.teaql.data.meta.EntityMetaFactory;
import io.teaql.data.meta.SimpleEntityMetaFactory;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import javax.sql.DataSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.MethodParameter;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;
import org.springframework.web.reactive.BindingContext;
import org.springframework.web.reactive.config.WebFluxConfigurer;
import org.springframework.web.reactive.result.method.annotation.ArgumentResolverConfigurer;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import reactor.core.publisher.Mono;

@Configuration
@EnableConfigurationProperties(DataSourceProperties.class)
@Order(Ordered.HIGHEST_PRECEDENCE)
public class TQLAutoConfiguration {

  // copy from spring jdbc, we only want the data source injection here
  @Configuration(proxyBeanMethods = false)
  @ConditionalOnClass(HikariDataSource.class)
  @ConditionalOnMissingBean(DataSource.class)
  @ConditionalOnProperty(
      name = "spring.datasource.type",
      havingValue = "com.zaxxer.hikari.HikariDataSource",
      matchIfMissing = true)
  static class Hikari {
    @Bean
    @ConfigurationProperties(prefix = "spring.datasource.hikari")
    HikariDataSource dataSource(DataSourceProperties properties) {
      HikariDataSource dataSource =
          properties.initializeDataSourceBuilder().type(HikariDataSource.class).build();
      if (StringUtils.hasText(properties.getName())) {
        dataSource.setPoolName(properties.getName());
      }
      return dataSource;
    }
  }

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
      return userContext;
    }

    @Bean
    @ConditionalOnMissingBean
    public WebMvcConfigurer webMvcConfigurer(TQLContextResolver tqlResolver) {
      return new WebMvcConfigurer() {
        @Override
        public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
          resolvers.add(tqlResolver);
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
}
