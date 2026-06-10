module io.teaql {
    requires io.teaql.utils;
    requires org.slf4j;
    requires com.fasterxml.jackson.core;
    requires com.fasterxml.jackson.databind;
    requires java.desktop;
    requires java.sql;

    // === 生成代码需要的公开 API ===
    exports io.teaql.data;
    exports io.teaql.data.checker;
    exports io.teaql.data.criteria;
    exports io.teaql.data.meta;
    exports io.teaql.data.translation;
    exports io.teaql.data.web;
    exports io.teaql.data.value;
    exports io.teaql.data.lock;
    exports io.teaql.data.log;

    // === 内部实现，精确授权 ===
    exports io.teaql.data.repository to io.teaql.sql, io.teaql.memory, io.teaql.android;
    exports io.teaql.data.internal to io.teaql.autoconfigure, io.teaql.sql;
}
