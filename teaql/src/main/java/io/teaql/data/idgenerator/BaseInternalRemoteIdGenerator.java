package io.teaql.data.idgenerator;

import io.teaql.data.Entity;
import io.teaql.data.InternalIdGenerator;
import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;

import java.util.List;
class TestEntity implements Entity{

    @Override
    public Long getId() {
        return null;
    }

    @Override
    public void setId(Long id) {

    }

    @Override
    public Long getVersion() {
        return null;
    }

    @Override
    public void setVersion(Long id) {

    }

    @Override
    public String typeName() {
        return Entity.super.typeName();
    }

    @Override
    public boolean newItem() {
        return false;
    }

    @Override
    public boolean updateItem() {
        return false;
    }

    @Override
    public boolean deleteItem() {
        return false;
    }

    @Override
    public boolean needPersist() {
        return false;
    }

    @Override
    public List<String> getUpdatedProperties() {
        return null;
    }

    @Override
    public void addRelation(String relationName, Entity value) {

    }

    @Override
    public void addDynamicProperty(String propertyName, Object value) {

    }

    @Override
    public void appendDynamicProperty(String propertyName, Object value) {

    }

    @Override
    public <T> T getDynamicProperty(String propertyName) {
        return null;
    }
}
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



        Long id=new BaseInternalRemoteIdGenerator().generateId(new TestEntity());
        System.out.println(id);


    }
}
