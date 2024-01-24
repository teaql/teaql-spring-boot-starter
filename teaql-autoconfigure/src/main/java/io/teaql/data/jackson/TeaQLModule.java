package io.teaql.data.jackson;

import com.fasterxml.jackson.databind.BeanDescription;
import com.fasterxml.jackson.databind.DeserializationConfig;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.deser.BeanDeserializerModifier;
import com.fasterxml.jackson.databind.module.SimpleModule;
import io.teaql.data.SmartList;

public class TeaQLModule extends SimpleModule {
  public static final TeaQLModule INSTANCE = new TeaQLModule();

  private TeaQLModule() {
    super("TeaQL");
    addSerializer(SmartList.class, new SmartListAsListSerializer(SmartList.class));
    setDeserializerModifier(
        new BeanDeserializerModifier() {
          @Override
          public JsonDeserializer<?> modifyDeserializer(
              DeserializationConfig config,
              BeanDescription beanDesc,
              JsonDeserializer<?> deserializer) {
            Class<?> beanClass = beanDesc.getBeanClass();
            if (beanClass.equals(SmartList.class)) {
              return new ListAsSmartListDeserializer<>(beanDesc.getType());
            }
            return super.modifyDeserializer(config, beanDesc, deserializer);
          }
        });
  }
}
