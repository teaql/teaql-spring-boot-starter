package io.teaql.data.idgenerator;

import io.teaql.data.BaseEntity;
import io.teaql.data.Entity;
import io.teaql.data.InternalIdGenerator;
import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;

import java.util.List;

public class BaseInternalRemoteIdGenerator implements InternalIdGenerator {


    @Override
    public Long generateId(Entity baseEntity) {



        RestTemplate restTemplate = new RestTemplate();

        String url = System.getProperty("id-gen-service-url","http://localhost:8080/genId");

        String body = "{\"typeName\":\""+baseEntity.typeName()+"\"}";

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        HttpEntity<String> requestEntity = new HttpEntity<>(body, headers);

        ResponseEntity<RemoteIdGenResponse> responseEntity = restTemplate.exchange(url, HttpMethod.POST, requestEntity, RemoteIdGenResponse.class);
        return responseEntity.getBody().getCurrent();
    }

    public static void main(String[] args) {



        Long id=new BaseInternalRemoteIdGenerator().generateId(new BaseEntity());
        System.out.println(id);


    }
}
