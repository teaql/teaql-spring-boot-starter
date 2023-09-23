package io.teaql.data.flux;

import io.teaql.data.RequestHolder;
import io.teaql.data.UserContext;
import io.teaql.data.web.UserContextInitializer;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

public class FluxInitializer implements UserContextInitializer {
  @Override
  public boolean support(Object request) {
    return request instanceof ServerWebExchange;
  }

  @Override
  public void init(UserContext userContext, Object request) {
    if (request instanceof ServerWebExchange exchange) {
      ServerHttpRequest serverHttpRequest = exchange.getRequest();
      userContext.put(
          UserContext.REQUEST_HOLDER,
          new RequestHolder() {

            @Override
            public String getHeader(String name) {
              return serverHttpRequest.getHeaders().getFirst(name);
            }

            @Override
            public byte[] getPart(String name) {
              return null;
            }

            @Override
            public String getParameter(String name) {
              return null;
            }

            @Override
            public byte[] getBodyBytes() {
              return new byte[0];
            }
          });
    }
  }
}
