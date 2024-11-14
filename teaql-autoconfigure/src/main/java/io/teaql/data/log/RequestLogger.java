package io.teaql.data.log;

import io.teaql.data.UserContext;
import io.teaql.data.web.UserContextInitializer;
import java.util.List;
import org.springframework.core.Ordered;

public class RequestLogger implements UserContextInitializer, Ordered {

  @Override
  public boolean support(Object request) {
    return true;
  }

  @Override
  public void init(UserContext userContext, Object request) {
    userContext.debug(
        Markers.HTTP_SHORT_REQUEST, "{} {}", userContext.method(), userContext.requestUri());
    List<String> headerNames = userContext.getHeaderNames();
    for (String headerName : headerNames) {
      userContext.debug(
          Markers.HTTP_REQUEST, "HEADER {}={}", headerName, userContext.getHeader(headerName));
    }

    List<String> parameterNames = userContext.getParameterNames();
    for (String parameterName : parameterNames) {
      userContext.debug(
          Markers.HTTP_SHORT_REQUEST,
          "PARAM {}={}",
          parameterName,
          userContext.getParameter(parameterName));
    }

    byte[] bodyBytes = userContext.getBodyBytes();
    if (bodyBytes != null) {
      String body = new String(bodyBytes);
      if (body.length() < 1000) {
        userContext.debug(Markers.HTTP_SHORT_REQUEST, "BODY: {}", body);
      } else {
        userContext.debug(Markers.HTTP_REQUEST, "BODY: {}", body);
      }
    }
  }

  @Override
  public int getOrder() {
    return Integer.MAX_VALUE;
  }
}
