package io.teaql.data.jackson;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.teaql.data.BaseEntity;
import io.teaql.data.web.WebResponse;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.HashMap;

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

    @Test
    void webResponseDataRoundTripsWithFacets() throws Exception {
        Product product = new Product();
        product.setId(7L);
        product.setVersion(3L);
        product.setName("USB-C Cable");

        Product facetProduct = new Product();
        facetProduct.setId(9L);
        facetProduct.setVersion(1L);
        facetProduct.setName("Facet Item");

        io.teaql.data.SmartList<Product> facetList = new io.teaql.data.SmartList<>();
        facetList.add(facetProduct);

        io.teaql.data.SmartList<Product> parentList = new io.teaql.data.SmartList<>();
        parentList.add(product);
        parentList.addFacet("status", facetList);

        // Verify getter/mutator APIs on SmartList
        assertNotNull(parentList.getFacets());
        assertTrue(parentList.getFacets().containsKey("status"));
        assertEquals(1, parentList.getFacet("status").size());

        String json = mapper.writeValueAsString(WebResponse.of(parentList));

        assertTrue(json.contains("\"facets\""));
        assertTrue(json.contains("\"status\""));
        assertTrue(json.contains("\"name\":\"Facet Item\""));

        WebResponse response = mapper.readValue(json, WebResponse.class);
        assertNotNull(response.getFacets());
        assertTrue(response.getFacets().containsKey("status"));
        
        io.teaql.data.SmartList deserializedFacet = response.getFacets().get("status");
        assertNotNull(deserializedFacet);
        assertEquals(1, deserializedFacet.size());

        // Test removing and clearing facets
        io.teaql.data.SmartList removed = parentList.removeFacet("status");
        assertNotNull(removed);
        assertTrue(parentList.getFacets().isEmpty());

        parentList.addFacet("status", facetList);
        parentList.clearFacets();
        assertTrue(parentList.getFacets().isEmpty());
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
