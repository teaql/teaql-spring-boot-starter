package io.teaql.data;

import java.util.List;

public interface RequestHolder {

    String method();

    String getHeader(String name);

    List<String> getHeaderNames();

    byte[] getPart(String name);

    List<String> getParameterNames();

    String getParameter(String name);

    byte[] getBodyBytes();

    String requestUri();

    String getRemoteAddress();
}
