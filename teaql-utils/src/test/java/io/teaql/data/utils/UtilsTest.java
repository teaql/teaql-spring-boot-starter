package io.teaql.data.utils;

import org.junit.jupiter.api.Test;
import java.io.ByteArrayInputStream;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

public class UtilsTest {

    // 1. Core Stateful Collections & Text Builders
    @Test
    public void testCacheAndTimedCache() {
        Cache<String, String> cache = CacheUtil.newTimedCache(5000);
        cache.put("key1", "value1");
        assertTrue(cache.containsKey("key1"));
        assertEquals("value1", cache.get("key1"));

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

        cache.put("c", 3);
        assertNull(cache.get("a")); // Evicted
        assertEquals(2, cache.get("b"));
        assertEquals(3, cache.get("c"));
    }

    @Test
    public void testCaseInsensitiveMap() {
        Map<String, String> map = new CaseInsensitiveMap<>();
        map.put("helloWorld", "value1");
        
        assertTrue(map.containsKey("helloworld"));
        assertTrue(map.containsKey("HELLOWORLD"));
        assertEquals("value1", map.get("helloworld"));
        
        map.remove("HELLOWORLD");
        assertFalse(map.containsKey("helloWorld"));
    }

    @Test
    public void testRowKeyTable() {
        RowKeyTable<String, String, Integer> table = new RowKeyTable<>();
        table.put("row1", "col1", 100);
        table.put("row1", "col2", 200);

        assertEquals(100, table.get("row1", "col1"));
        assertEquals(200, table.get("row1", "col2"));

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

        sb.clear();
        assertEquals(0, sb.length());
        assertEquals("", sb.toString());
    }

    // 2. Static Utilities Validation
    @Test
    public void testArrayUtil() {
        String[] arr = {"a", "b", "c"};
        assertTrue(ArrayUtil.contains(arr, "b"));
        assertFalse(ArrayUtil.contains(arr, "z"));
        assertEquals("b", ArrayUtil.get(arr, 1));
        assertTrue(ArrayUtil.isArray(arr));
        assertEquals(3, ArrayUtil.length(arr));
        
        String[] sub = ArrayUtil.sub(arr, 1, 3);
        assertEquals(2, sub.length);
        assertEquals("b", sub[0]);
        assertEquals("c", sub[1]);
    }

    @Test
    public void testBase64AndEncoder() {
        String original = "TeaQL Utilities";
        String encoded = Base64.encode(original);
        assertNotNull(encoded);
        
        String decoded = Base64.decodeStr(encoded);
        assertEquals(original, decoded);

        String urlSafe = Base64Encoder.encodeUrlSafe(original.getBytes());
        assertNotNull(urlSafe);
    }

    @Test
    public void testBeanUtil() {
        Person person = new Person();
        person.setName("Alice");
        person.setAge(25);

        Map<String, Object> map = BeanUtil.beanToMap(person);
        assertEquals("Alice", map.get("name"));
        assertEquals(25, map.get("age"));

        Person copied = BeanUtil.toBean(map, Person.class);
        assertEquals("Alice", copied.getName());
        assertEquals(25, copied.getAge());

        BeanUtil.setProperty(copied, "name", "Bob");
        assertEquals("Bob", BeanUtil.getProperty(copied, "name"));
    }

    @Test
    public void testBooleanAndCharUtil() {
        assertTrue(BooleanUtil.toBoolean("true"));
        assertFalse(BooleanUtil.toBoolean("false"));

        assertTrue(CharUtil.isLetter('A'));
        assertFalse(CharUtil.isLetter('1'));
    }

    @Test
    public void testClassUtil() {
        Method[] methods = ClassUtil.getPublicMethods(Person.class);
        assertTrue(methods.length > 0);

        assertTrue(ClassUtil.isAssignable(List.class, ArrayList.class));
        assertFalse(ClassUtil.isAssignable(ArrayList.class, List.class));

        assertTrue(ClassUtil.isInterface(List.class));
        assertFalse(ClassUtil.isInterface(Person.class));

        assertTrue(ClassUtil.isSimpleValueType(String.class));
        assertTrue(ClassUtil.isSimpleValueType(int.class));
        assertFalse(ClassUtil.isSimpleValueType(Person.class));

        Class<?> loaded = ClassUtil.loadClass("io.teaql.data.utils.UtilsTest$Person");
        assertEquals(Person.class, loaded);
    }

    @Test
    public void testCollectionsAndStreams() {
        List<Person> list = Arrays.asList(
            new Person("Alice", 20),
            new Person("Bob", 30),
            new Person("Charlie", 20)
        );

        // CollStreamUtil
        Map<Integer, List<Person>> grouped = CollStreamUtil.groupByKey(list, Person::getAge);
        assertEquals(2, grouped.get(20).size());
        assertEquals(1, grouped.get(30).size());

        Map<String, Person> mapped = CollStreamUtil.toIdentityMap(list, Person::getName);
        assertEquals(20, mapped.get("Alice").getAge());

        // CollUtil / CollectionUtil
        Person first = CollUtil.getFirst(list);
        assertEquals("Alice", first.getName());

        assertTrue(CollectionUtil.isEmpty((Collection<?>) null));
        assertTrue(CollectionUtil.isEmpty(new ArrayList<>()));
        assertFalse(CollectionUtil.isEmpty(list));

        assertEquals(3, CollectionUtil.size(list));
        assertEquals("Alice,Bob,Charlie", CollectionUtil.join(list.stream().map(Person::getName).collect(Collectors.toList()), ","));
    }

    @Test
    public void testCompareAndConvert() {
        assertTrue(CompareUtil.compare(1, 2) < 0);
        assertEquals(0, CompareUtil.compare(5, 5));
        assertTrue(CompareUtil.compare("z", "a") > 0);

        assertEquals(Integer.valueOf(123), Convert.convert(Integer.class, "123"));
        assertEquals(BigDecimal.valueOf(45.67), Convert.convert(BigDecimal.class, "45.67"));
    }

    @Test
    public void testDateAndTemporal() {
        Date date = new Date(1716700000000L); // May 2024
        LocalDateTime ldt = DateUtil.toLocalDateTime(date);
        assertNotNull(ldt);

        long epoch = TemporalAccessorUtil.toEpochMilli(ldt);
        assertEquals(1716700000000L, epoch);

        String format = LocalDateTimeUtil.formatNormal(ldt);
        assertNotNull(format);
    }

    @Test
    public void testIdUtil() {
        String uuid = IdUtil.fastSimpleUUID();
        assertEquals(32, uuid.length());
        assertFalse(uuid.contains("-"));

        long nextId = IdUtil.getSnowflakeNextId();
        assertTrue(nextId > 0);

        String nextIdStr = IdUtil.getSnowflakeNextIdStr();
        assertNotNull(nextIdStr);
    }

    @Test
    public void testMapUtil() {
        Map<String, Object> map = MapUtil.of("key1", "value1");
        assertEquals("value1", map.get("key1"));

        Map<Object, Object> builderMap = MapUtil.builder()
            .put("k1", 1)
            .put("k2", true)
            .build();
        assertEquals(1, builderMap.get("k1"));
        assertTrue(MapUtil.getBool(builderMap, "k2"));

        assertFalse(builderMap.isEmpty());
        assertTrue(MapUtil.empty().isEmpty());
    }

    @Test
    public void testNamingCase() {
        assertEquals("helloWorld", NamingCase.toCamelCase("hello_world"));
        assertEquals("HelloWorld", NamingCase.toPascalCase("hello_world"));
        assertEquals("hello_world", NamingCase.toUnderlineCase("helloWorld"));
    }

    @Test
    public void testNumberUtil() {
        BigDecimal a = new BigDecimal("10.50");
        BigDecimal b = new BigDecimal("5.25");
        
        assertEquals(new BigDecimal("15.75"), NumberUtil.add(a, b));
        assertTrue(NumberUtil.isGreater(a, b));
        assertTrue(NumberUtil.isLess(b, a));
        
        assertEquals(100, NumberUtil.parseNumber("100").intValue());
        assertEquals(new BigDecimal("99.9"), NumberUtil.toBigDecimal("99.9"));
    }

    @Test
    public void testObjAndObjectUtil() {
        assertTrue(ObjUtil.isEmpty(null));
        assertTrue(ObjUtil.isEmpty(""));
        
        assertTrue(ObjectUtil.isNotNull("not null"));
        assertTrue(ObjectUtil.isNull(null));
        
        assertTrue(ObjectUtil.equals("a", "a"));
        assertFalse(ObjectUtil.equals("a", "b"));
        
        assertEquals(5, ObjectUtil.length("hello"));
        assertEquals(3, ObjectUtil.length(new int[]{1, 2, 3}));
    }

    @Test
    public void testReflectUtil() {
        Person p = ReflectUtil.newInstance(Person.class);
        assertNotNull(p);

        ReflectUtil.invoke(p, "setName", "Charlie");
        assertEquals("Charlie", p.getName());

        java.lang.reflect.Field nameField = ReflectUtil.getField(Person.class, "name");
        assertNotNull(nameField);
    }

    @Test
    public void testURLCodec() {
        String original = "Hello World! @ 2026";
        String encoded = URLEncodeUtil.encode(original);
        assertNotEquals(original, encoded);

        String decoded = URLDecoder.decode(encoded, CharsetUtil.CHARSET_UTF_8);
        assertEquals(original, decoded);
    }

    @Test
    public void testZipUtil() {
        byte[] original = "Compress me compress me compress me".getBytes();
        byte[] compressed = ZipUtil.gzip(original);
        assertTrue(compressed.length > 0);

        byte[] decompressed = ZipUtil.unGzip(compressed);
        assertArrayEquals(original, decompressed);
    }

    @Test
    public void testOptNullBasicTypeFromObjectGetter() {
        OptNullBasicTypeFromObjectGetter<String> getter = new OptNullBasicTypeFromObjectGetter<String>() {
            @Override
            public Object getObj(String key, Object defaultValue) {
                if ("str".equals(key)) return "hello";
                if ("num".equals(key)) return 123;
                if ("bool".equals(key)) return true;
                return defaultValue;
            }
        };

        assertEquals("hello", getter.getStr("str"));
        assertEquals(Integer.valueOf(123), getter.getInt("num"));
        assertTrue(getter.getBool("bool"));
        assertNull(getter.getStr("missing"));
    }

    @Test
    public void testJSONUtilToBean() {
        String json = "{\"name\":\"John\",\"age\":30}";
        
        Person person = JSONUtil.toBean(json, Person.class);
        assertNotNull(person);
        assertEquals("John", person.getName());
        assertEquals(30, person.getAge());

        Map<String, Object> map = JSONUtil.toBean(json, new TypeReference<Map<String, Object>>() {}, true);
        assertNotNull(map);
        assertEquals("John", map.get("name"));
    }

    // Test helper classes
    public static class Person {
        private String name;
        private int age;

        public Person() {}

        public Person(String name, int age) {
            this.name = name;
            this.age = age;
        }

        public String getName() { return name; }
        public void setName(String name) { this.name = name; }
        public int getAge() { return age; }
        public void setAge(int age) { this.age = age; }
    }
}
