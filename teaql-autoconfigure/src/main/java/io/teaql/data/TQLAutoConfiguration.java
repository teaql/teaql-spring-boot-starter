package io.teaql.data;

import io.teaql.data.meta.EntityMetaFactory;
import io.teaql.data.meta.SimpleEntityMetaFactory;
import java.util.List;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.reactive.config.WebFluxConfigurer;
import org.springframework.web.reactive.result.method.annotation.ArgumentResolverConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class TQLAutoConfiguration {

  @Bean
  @ConditionalOnMissingBean
  @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
  public TQLContextResolver userContextResolver() {
    return new TQLContextResolver(dataConfig());
  }

  @Bean
  @ConditionalOnMissingBean
  @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.REACTIVE)
  public TQLReactiveContextResolver reactiveUserContextResolver() {
    return new TQLReactiveContextResolver(dataConfig());
  }

  @Bean
  @ConditionalOnMissingBean
  public EntityMetaFactory entityMetaFactory() {
    return new SimpleEntityMetaFactory();
  }

  @Bean
  @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
  public WebMvcConfigurer webMvcConfigurer(List<HandlerMethodArgumentResolver> custom) {
    return new WebMvcConfigurer() {
      @Override
      public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
        resolvers.addAll(custom);
      }
    };
  }

  @Bean
  @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.REACTIVE)
  public WebFluxConfigurer reactiveConfigure(
      List<org.springframework.web.reactive.result.method.HandlerMethodArgumentResolver> custom) {
    return new WebFluxConfigurer() {
      @Override
      public void configureArgumentResolvers(ArgumentResolverConfigurer configure) {
        for (org.springframework.web.reactive.result.method.HandlerMethodArgumentResolver
            handlerMethodArgumentResolver : custom) {
          configure.addCustomResolver(handlerMethodArgumentResolver);
        }
      }
    };
  }

  @Bean
  @ConfigurationProperties(prefix = "teaql")
  public DataConfigProperties dataConfig() {
    return new DataConfigProperties();
  }
}
