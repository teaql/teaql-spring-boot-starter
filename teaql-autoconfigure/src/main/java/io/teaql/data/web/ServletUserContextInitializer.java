package io.teaql.data.web;

import cn.hutool.core.io.IoUtil;
import io.teaql.data.RequestHolder;
import io.teaql.data.UserContext;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletInputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.Part;
import java.io.IOException;
import java.io.InputStream;

public class ServletUserContextInitializer implements UserContextInitializer {

  @Override
  public boolean support(Object request) {
    return request instanceof HttpServletRequest;
  }

  @Override
  public void init(UserContext userContext, Object request) {
    if (request instanceof HttpServletRequest httpRequest) {
      userContext.put(
          UserContext.REQUEST_HOLDER,
          new RequestHolder() {
            @Override
            public String getHeader(String name) {
              return httpRequest.getHeader(name);
            }

            @Override
            public byte[] getPart(String name) {
              try {
                Part part = httpRequest.getPart(name);
                InputStream inputStream = part.getInputStream();
                return IoUtil.readBytes(inputStream);
              } catch (IOException pE) {
                throw new RuntimeException(pE);
              } catch (ServletException pE) {
                throw new RuntimeException(pE);
              }
            }

            @Override
            public String getParameter(String name) {
              return httpRequest.getParameter(name);
            }

            @Override
            public byte[] getBodyBytes() {
              ServletInputStream inputStream;
              try {
                inputStream = httpRequest.getInputStream();
              } catch (IOException pE) {
                throw new RuntimeException(pE);
              }
              return IoUtil.readBytes(inputStream);
            }
          });
    }
  }
}
