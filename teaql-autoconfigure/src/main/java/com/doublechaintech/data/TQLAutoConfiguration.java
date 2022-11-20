package com.doublechaintech.data;

import cn.hutool.extra.spring.SpringUtil;
import com.doublechaintech.data.meta.EntityMetaFactory;
import com.doublechaintech.data.meta.SimpleEntityMetaFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.util.List;

@Configuration
@Import(SpringUtil.class)
public class TQLAutoConfiguration implements WebMvcConfigurer {

  @Bean
  @ConditionalOnMissingBean
  public TQLContextResolver userContextResolver() {
    return new TQLContextResolver();
  }

  @Bean
  @ConditionalOnMissingBean
  public EntityMetaFactory entityMetaFactory() {
    return new SimpleEntityMetaFactory();
  }

  @Override
  public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
    resolvers.add(userContextResolver());
  }
}
