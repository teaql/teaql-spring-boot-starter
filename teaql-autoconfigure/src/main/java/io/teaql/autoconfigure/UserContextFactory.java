package io.teaql.autoconfigure;

import io.teaql.data.UserContext;

public interface UserContextFactory {
    UserContext create(Object request);
}
