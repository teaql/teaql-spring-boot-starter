module io.teaql.sql {
    requires io.teaql;
    requires io.teaql.utils;
    requires spring.jdbc;
    requires spring.tx;
    requires java.sql;
    requires org.slf4j;
    requires com.fasterxml.jackson.core;
    requires com.fasterxml.jackson.databind;

    exports io.teaql.data.sql;
    exports io.teaql.data.sql.expression;
}
