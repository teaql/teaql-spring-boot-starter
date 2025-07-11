package io.teaql.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.redisson.api.RedissonClient;
import org.redisson.codec.JsonJacksonCodec;
import org.redisson.spring.starter.RedissonAutoConfigurationCustomizer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.autoconfigure.jackson.Jackson2ObjectMapperBuilderCustomizer;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.MethodParameter;
import org.springframework.core.annotation.AnnotationAwareOrderComparator;
import org.springframework.core.annotation.Order;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConverter;
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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.codec.Base64;
import cn.hutool.core.collection.CollStreamUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.extra.spring.SpringUtil;
import cn.hutool.json.JSONUtil;

import io.teaql.data.checker.Checker;
import io.teaql.data.jackson.TeaQLModule;
import io.teaql.data.lock.LockService;
import io.teaql.data.lock.LockServiceImpl;
import io.teaql.data.meta.EntityMetaFactory;
import io.teaql.data.meta.SimpleEntityMetaFactory;
import io.teaql.data.redis.RedisStore;
import io.teaql.data.translation.Translator;
import io.teaql.data.web.BlobObjectMessageConverter;
import io.teaql.data.web.MultiReadFilter;
import io.teaql.data.web.ServletUserContextInitializer;
import io.teaql.data.web.UITemplateRender;
import io.teaql.data.web.UserContextInitializer;
import jakarta.servlet.Filter;
import reactor.core.publisher.Mono;

@Configuration
public class TQLAutoConfiguration {

    @Bean("checkers")
    public Map<String, Checker> checkers(List<Checker> checkers) {
        return CollStreamUtil.toIdentityMap(checkers, Checker::type);
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
    @ConditionalOnMissingBean
    public Translator translator() {
        return Translator.NOOP;
    }

    @Bean
    @ConditionalOnMissingBean(name = "templateRender")
    public UITemplateRender templateRender() {
        return new UITemplateRender();
    }

    @Bean
    public RedissonAutoConfigurationCustomizer codec() {
        return config -> {
            ObjectMapper objectMapper = new ObjectMapper();
            objectMapper.enableDefaultTyping(
                    ObjectMapper.DefaultTyping.EVERYTHING, JsonTypeInfo.As.PROPERTY);
            config.setCodec(new JsonJacksonCodec(objectMapper));
        };
    }

    @Bean
    @ConditionalOnMissingBean
    public DataStore dataStore(RedissonClient redissonClient) {
        return new RedisStore(redissonClient);
    }

    @Bean
    @ConditionalOnMissingBean
    public LockService lockService() {
        return new LockServiceImpl();
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
                        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
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
                        ArrayList<T> list = new ArrayList<>(beansOfType.values());
                        list.sort(AnnotationAwareOrderComparator.INSTANCE);
                        return list;
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
    @ConditionalOnMissingBean(name = "blobObjectMessageConverter")
    public BlobObjectMessageConverter blobObjectMessageConverter() {
        return new BlobObjectMessageConverter();
    }

    @Bean("multiReadFilter")
    @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
    @Order
    public Filter multiReadRequest() {
        return new MultiReadFilter();
    }

    @Bean
    @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
    @Order
    public UserContextInitializer servletInitializer() {
        return new ServletUserContextInitializer();
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
        public WebMvcConfigurer tqlConfigure(
                TQLContextResolver tqlResolver, BlobObjectMessageConverter blobObjectMessageConverter) {
            return new WebMvcConfigurer() {
                @Override
                public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
                    resolvers.add(tqlResolver);
                }

                @Override
                public void extendMessageConverters(List<HttpMessageConverter<?>> converters) {
                    converters.removeIf(c -> c == blobObjectMessageConverter);
                    converters.add(0, blobObjectMessageConverter);
                }
            };
        }
    }

    @ControllerAdvice
    @ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
    public static class TeaQLResponseAdvice implements ResponseBodyAdvice {
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
            handleXClass(body, response);
            handleToast(body, response);
            return body;
        }

        private void handleXClass(Object body, ServerHttpResponse response) {
            String xClass = response.getHeaders().getFirst(UserContext.X_CLASS);
            if (ObjectUtil.isEmpty(xClass) && body != null) {
                response.getHeaders().set(UserContext.X_CLASS, body.getClass().getName());
            }
        }

        private void handleToast(Object body, ServerHttpResponse response) {
            String command = response.getHeaders().getFirst("command");
            if (command == null) {
                String toast = response.getHeaders().getFirst("toast");
                response.getHeaders().remove("toast");
                if (toast != null) {
                    try {
                        Object toastObj = JSONUtil.toBean(Base64.decodeStr(toast), Map.class);
                        BeanUtil.setProperty(body, "toast", toastObj);
                        Object playSound = BeanUtil.getProperty(toastObj, "playSound");
                        if (playSound != null) {
                            BeanUtil.setProperty(body, "playSound", playSound);
                        }
                    }
                    catch (Exception e) {
                        // ignore toast setting
                    }
                }
            }
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
