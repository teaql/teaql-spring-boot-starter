module io.teaql.graphql {
    requires io.teaql;
    requires io.teaql.utils;
    requires spring.context;
    requires spring.boot.autoconfigure;
    requires com.fasterxml.jackson.core;
    requires com.fasterxml.jackson.databind;
    requires com.graphqljava;
    requires com.graphqljava.extendedscalars;

    exports io.teaql.data.graphql;
}
