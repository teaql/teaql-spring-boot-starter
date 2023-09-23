package io.teaql.data.web;

import io.teaql.data.UserContext;

public interface UserContextInitializer {

  boolean support(Object request);

  void init(UserContext userContext, Object request);
}
