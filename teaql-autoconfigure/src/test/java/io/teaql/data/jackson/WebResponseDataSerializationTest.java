package io.teaql.data.jackson;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.teaql.data.BaseEntity;
import io.teaql.data.web.WebResponse;
import org.junit.jupiter.api.Test;

class WebResponseDataSerializationTest {

    private final ObjectMapper mapper = new ObjectMapper().registerModule(TeaQLModule.INSTANCE);

    @Test
    void webResponseDataRoundTripsWithEntityFields() throws Exception {
        Product product = new Product();
        product.setId(7L);
        product.setVersion(3L);
        product.setName("USB-C Cable");

        String json = mapper.writeValueAsString(WebResponse.of(product));

        assertTrue(json.contains("\"data\""));
        assertTrue(json.contains("\"name\":\"USB-C Cable\""));

        WebResponse response = mapper.readValue(json, WebResponse.class);

        assertEquals(0, response.getResultCode());
        assertEquals("YES", response.getStatus());
        assertEquals(1, response.getRecordCount());
        assertEquals(1, response.getData().size());

        BaseEntity entity = response.getData().get(0);
        assertNotNull(entity);
        assertEquals(7L, entity.getId());
        assertEquals(3L, entity.getVersion());
        assertEquals("USB-C Cable", entity.getAdditionalInfo().get("name"));
    }

    static class Product extends BaseEntity {
        private String name;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }
    }
}
