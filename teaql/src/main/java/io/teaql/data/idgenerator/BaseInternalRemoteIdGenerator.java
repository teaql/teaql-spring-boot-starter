package io.teaql.data.idgenerator;

import cn.hutool.http.HttpUtil;
import cn.hutool.json.JSONUtil;
import io.teaql.data.Entity;
import io.teaql.data.InternalIdGenerator;

public class BaseInternalRemoteIdGenerator implements InternalIdGenerator {

  @Override
  public Long generateId(Entity baseEntity) {
    String url = System.getProperty("id-gen-service-url", "http://localhost:8080/genId");
    String body = "{\"typeName\":\"" + baseEntity.typeName() + "\"}";
    String response = HttpUtil.post(url, body);
    RemoteIdGenResponse result = JSONUtil.toBean(response, RemoteIdGenResponse.class);
    return result.getCurrent();
  }
}
