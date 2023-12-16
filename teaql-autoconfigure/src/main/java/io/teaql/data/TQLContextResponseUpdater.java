package io.teaql.data;

import static io.teaql.data.TQLAutoConfiguration.TQL_CONTEXT;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.Map;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

public class TQLContextResponseUpdater implements HandlerInterceptor {
  @Override
  public void postHandle(
      HttpServletRequest request,
      HttpServletResponse response,
      Object handler,
      ModelAndView modelAndView)
      throws Exception {
    UserContext ctx = (UserContext) modelAndView.getModel().get(TQL_CONTEXT);
    if (ctx == null) {
      return;
    }
    Map<String, String> headers = ctx.getResponseHeaders();
    headers.forEach(
        (k, v) -> {
          response.setHeader(k, v);
        });
  }
}
