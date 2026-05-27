package io.teaql.data;

public interface UserContextFactory {
    UserContext create(Object request);
}
