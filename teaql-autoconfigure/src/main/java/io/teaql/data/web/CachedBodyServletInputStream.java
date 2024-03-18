package io.teaql.data.web;

import jakarta.servlet.ReadListener;
import jakarta.servlet.ServletInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

public class CachedBodyServletInputStream extends ServletInputStream {
  private InputStream cachedBodyInputStream;

  public CachedBodyServletInputStream(byte[] cachedBody) {
    this.cachedBodyInputStream = new ByteArrayInputStream(cachedBody);
  }

  @Override
  public int read() throws IOException {
    return cachedBodyInputStream.read();
  }

  @Override
  public boolean isFinished() {
    try {
      return cachedBodyInputStream.available() == 0;
    } catch (IOException pE) {
      throw new RuntimeException(pE);
    }
  }

  @Override
  public boolean isReady() {
    return true;
  }

  @Override
  public void setReadListener(ReadListener listener) {}
}
