package io.teaql.data.web;

import io.teaql.data.UserContext;
import javax.servlet.http.HttpServletRequest;
import org.springframework.web.servlet.handler.DispatcherServletWebRequest;

public class ServletUserContextInitializer implements UserContextInitializer {

  @Override
  public void init(UserContext userContext, Object request) {
    if (request instanceof HttpServletRequest httpRequest) {
      DispatcherServletWebRequest webRequest = new DispatcherServletWebRequest(httpRequest);
    }
  }
}
