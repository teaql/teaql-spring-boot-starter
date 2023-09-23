package io.teaql.data.web;

import org.springframework.boot.web.servlet.filter.OrderedFilter;
import org.springframework.web.util.ContentCachingRequestWrapper;

import java.io.IOException;
import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;

public class MultiReadFilter implements OrderedFilter {
  @Override
  public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
      throws IOException, ServletException {
    if (request instanceof ContentCachingRequestWrapper){
      chain.doFilter(request, response);
    }else{
      chain.doFilter(new ContentCachingRequestWrapper((HttpServletRequest) request), response);
    }
  }

  @Override
  public int getOrder() {
    return 0;
  }
}
