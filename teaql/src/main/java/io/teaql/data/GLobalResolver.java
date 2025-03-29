package io.teaql.data;

public class GLobalResolver {
    public static TQLResolver GLOBAL_RESOLVER;

    public static void registerResolver(TQLResolver resolver) {
        GLOBAL_RESOLVER = resolver;
    }

    public static TQLResolver getGlobalResolver() {
        return GLOBAL_RESOLVER;
    }
}
