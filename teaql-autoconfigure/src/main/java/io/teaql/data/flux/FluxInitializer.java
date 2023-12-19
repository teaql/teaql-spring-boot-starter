package io.teaql.data.flux;

import io.teaql.data.RequestHolder;
import io.teaql.data.ResponseHolder;
import io.teaql.data.UserContext;
import io.teaql.data.web.UserContextInitializer;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;

public class FluxInitializer implements UserContextInitializer {
  @Override
  public boolean support(Object request) {
    return request instanceof ServerWebExchange;
  }

  @Override
  public void init(UserContext userContext, Object request) {
    if (request instanceof ServerWebExchange exchange) {
      ServerHttpRequest serverHttpRequest = exchange.getRequest();
      ServerHttpResponse serverHttpResponse = exchange.getResponse();
      userContext.put(
          UserContext.REQUEST_HOLDER,
          new RequestHolder() {

            @Override
            public String getHeader(String name) {
              return serverHttpRequest.getHeaders().getFirst(name);
            }

            @Override
            public byte[] getPart(String name) {
              Flux<DataBuffer> data =
                  exchange.getMultipartData().map(i -> i.getFirst(name).content()).block();
              return DataBufferUtils.join(data)
                  .map(
                      dataBuffer -> {
                        byte[] bytes = new byte[dataBuffer.readableByteCount()];
                        dataBuffer.read(bytes);
                        DataBufferUtils.release(dataBuffer);
                        return bytes;
                      })
                  .block();
            }

            @Override
            public String getParameter(String name) {
              String queryParam = serverHttpRequest.getQueryParams().getFirst(name);
              if (queryParam != null) {
                return queryParam;
              }
              return exchange.getFormData().map(i -> i.getFirst(name)).block();
            }

            @Override
            public byte[] getBodyBytes() {
              Flux<DataBuffer> body = serverHttpRequest.getBody();
              return DataBufferUtils.join(body)
                  .map(
                      dataBuffer -> {
                        byte[] bytes = new byte[dataBuffer.readableByteCount()];
                        dataBuffer.read(bytes);
                        DataBufferUtils.release(dataBuffer);
                        return bytes;
                      })
                  .block();
            }
          });

      userContext.put(
          UserContext.RESPONSE_HOLDER,
          new ResponseHolder() {
            @Override
            public void setHeader(String name, String value) {
              serverHttpResponse.getHeaders().add(name, value);
            }

            @Override
            public String getHeader(String name) {
              return serverHttpResponse.getHeaders().getFirst(name);
            }
          });
    }
  }
}
