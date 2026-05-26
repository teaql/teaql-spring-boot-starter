package io.teaql.data.utils;

import org.junit.jupiter.api.Test;
import java.util.Map;
import static org.junit.jupiter.api.Assertions.*;

public class UtilsTest {

    @Test
    public void testCacheAndTimedCache() {
        Cache<String, String> cache = CacheUtil.newTimedCache(5000);
        cache.put("key1", "value1");
        assertTrue(cache.containsKey("key1"));
        assertEquals("value1", cache.get("key1"));

        // Test Supplier get
        String val = cache.get("key2", () -> "default_val");
        assertEquals("default_val", val);
        assertEquals("default_val", cache.get("key2"));

        cache.remove("key1");
        assertFalse(cache.containsKey("key1"));
        assertNull(cache.get("key1"));
    }

    @Test
    public void testLRUCache() {
        Cache<String, Integer> cache = CacheUtil.newLRUCache(2, 5000);
        cache.put("a", 1);
        cache.put("b", 2);
        assertEquals(1, cache.get("a"));
        assertEquals(2, cache.get("b"));

        // Should evict oldest key "a" when putting "c" if capacity is strictly 2
        cache.put("c", 3);
        assertNull(cache.get("a")); // "a" should be evicted
        assertEquals(2, cache.get("b"));
        assertEquals(3, cache.get("c"));
    }

    @Test
    public void testCaseInsensitiveMap() {
        Map<String, String> map = new CaseInsensitiveMap<>();
        map.put("helloWorld", "value1");
        
        assertTrue(map.containsKey("helloworld"));
        assertTrue(map.containsKey("HELLOWORLD"));
        assertTrue(map.containsKey("helloWorld"));
        
        assertEquals("value1", map.get("helloworld"));
        assertEquals("value1", map.get("HELLOWORLD"));
        
        map.remove("HELLOWORLD");
        assertFalse(map.containsKey("helloWorld"));
    }

    @Test
    public void testRowKeyTable() {
        RowKeyTable<String, String, Integer> table = new RowKeyTable<>();
        table.put("row1", "col1", 100);
        table.put("row1", "col2", 200);
        table.put("row2", "col1", 300);

        assertEquals(100, table.get("row1", "col1"));
        assertEquals(200, table.get("row1", "col2"));
        assertEquals(300, table.get("row2", "col1"));
        assertNull(table.get("row2", "col2"));

        table.remove("row1", "col1");
        assertNull(table.get("row1", "col1"));
        assertEquals(200, table.get("row1", "col2"));
    }

    @Test
    public void testTypeReference() {
        TypeReference<Map<String, Double>> typeRef = new TypeReference<Map<String, Double>>() {};
        assertNotNull(typeRef.getType());
        assertTrue(typeRef.getType() instanceof java.lang.reflect.ParameterizedType);
    }

    @Test
    public void testStrBuilder() {
        StrBuilder sb = new StrBuilder();
        sb.append("Hello ").append("World").append('!');
        assertEquals("Hello World!", sb.toString());
        assertEquals(12, sb.length());
        assertEquals('e', sb.charAt(1));

        sb.clear();
        assertEquals(0, sb.length());
        assertEquals("", sb.toString());
    }

    @Test
    public void testStrUtilAndBuilder() {
        StrBuilder sb = StrUtil.strBuilder();
        assertNotNull(sb);
        sb.append("test");
        assertEquals("test", sb.toString());

        // Test static utility methods
        assertTrue(StrUtil.startWith("hello", "he"));
        assertTrue(StrUtil.startWithIgnoreCase("hello", "HE"));
        assertEquals("Hello", StrUtil.upperFirst("hello"));
    }

    @Test
    public void testJSONUtilToBean() {
        String json = "{\"name\":\"John\",\"age\":30}";
        
        // Test standard class deserialization
        Person person = JSONUtil.toBean(json, Person.class);
        assertNotNull(person);
        assertEquals("John", person.getName());
        assertEquals(30, person.getAge());

        // Test TypeReference deserialization
        Map<String, Object> map = JSONUtil.toBean(json, new TypeReference<Map<String, Object>>() {}, true);
        assertNotNull(map);
        assertEquals("John", map.get("name"));
    }

    public static class Person {
        private String name;
        private int age;

        public String getName() { return name; }
        public void setName(String name) { this.name = name; }
        public int getAge() { return age; }
        public void setAge(int age) { this.age = age; }
    }
}
