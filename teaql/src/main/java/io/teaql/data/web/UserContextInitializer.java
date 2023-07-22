package io.teaql.data.web;

import io.teaql.data.UserContext;

public interface UserContextInitializer {
    void init(UserContext userContext, Object request);
}
