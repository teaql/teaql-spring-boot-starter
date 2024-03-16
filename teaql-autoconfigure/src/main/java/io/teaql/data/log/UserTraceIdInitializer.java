package io.teaql.data.log;

import cn.hutool.core.util.IdUtil;
import cn.hutool.core.util.ObjectUtil;
import io.teaql.data.UserContext;
import io.teaql.data.web.UserContextInitializer;
import java.util.List;
import org.slf4j.MDC;
import org.springframework.core.PriorityOrdered;

public class UserTraceIdInitializer implements UserContextInitializer, PriorityOrdered {

  public static final String TRACE_ID = "TRACE_ID";
  public static final String TRACE_PREFIX = "TRACE_";
  public static final String TRACE_PATH = "TRACE_PATH";

  @Override
  public boolean support(Object request) {
    return true;
  }

  @Override
  public void init(UserContext userContext, Object request) {
    List<String> headerNames = userContext.getHeaderNames();
    for (String headerName : headerNames) {
      if (headerName.startsWith(TRACE_PREFIX)) {
        MDC.put(headerName, userContext.getHeader(headerName));
      }
    }
    List<String> parameterNames = userContext.getParameterNames();
    for (String parameterName : parameterNames) {
      if (parameterName.startsWith(TRACE_PREFIX)) {
        MDC.put(parameterName, userContext.getParameter(parameterName));
      }
    }
    String traceId = MDC.get(TRACE_ID);
    if (ObjectUtil.isEmpty(traceId)) {
      traceId = IdUtil.getSnowflakeNextIdStr();
      MDC.put(TRACE_ID, 'T' + traceId);
    }
    MDC.put(TRACE_PATH, userContext.requestUri());
  }

  @Override
  public int getOrder() {
    return Integer.MIN_VALUE + 1;
  }
}
