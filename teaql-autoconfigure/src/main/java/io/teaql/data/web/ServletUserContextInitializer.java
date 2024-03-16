package io.teaql.data.web;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.io.IoUtil;
import io.teaql.data.RequestHolder;
import io.teaql.data.ResponseHolder;
import io.teaql.data.UserContext;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletInputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.Part;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import org.springframework.core.PriorityOrdered;
import org.springframework.web.context.request.NativeWebRequest;

public class ServletUserContextInitializer implements UserContextInitializer, PriorityOrdered {

  public static final String USER_CONTEXT = "USER_CONTEXT";

  @Override
  public boolean support(Object request) {
    return request instanceof NativeWebRequest;
  }

  @Override
  public void init(UserContext userContext, Object request) {
    if (request instanceof NativeWebRequest nativeWebRequest) {
      if (nativeWebRequest.getNativeRequest() instanceof HttpServletRequest httpRequest) {
        httpRequest.setAttribute(USER_CONTEXT, userContext);
        userContext.put(
            UserContext.REQUEST_HOLDER,
            new RequestHolder() {

              @Override
              public String method() {
                return httpRequest.getMethod();
              }

              @Override
              public String getHeader(String name) {
                return httpRequest.getHeader(name);
              }

              @Override
              public List<String> getHeaderNames() {
                return ListUtil.toList(httpRequest.getHeaderNames().asIterator());
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
              public List<String> getParameterNames() {
                return ListUtil.toList(httpRequest.getParameterNames().asIterator());
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

              @Override
              public String requestUri() {
                String requestURI = httpRequest.getRequestURI();
                String contextPath = httpRequest.getContextPath();
                return requestURI.substring(contextPath.length());
              }
            });
      }

      if (nativeWebRequest.getNativeResponse() instanceof HttpServletResponse response)
        userContext.put(
            UserContext.RESPONSE_HOLDER,
            new ResponseHolder() {
              @Override
              public void setHeader(String name, String value) {
                response.setHeader(name, value);
              }

              @Override
              public String getHeader(String name) {
                return response.getHeader(name);
              }
            });
    }
  }

  @Override
  public int getOrder() {
    return Integer.MIN_VALUE;
  }
}
