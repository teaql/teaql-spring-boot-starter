package io.teaql.data;

public class TQLException extends RuntimeException {
  public TQLException() {}

  public TQLException(String message) {
    super(message);
  }

  public TQLException(String message, Throwable cause) {
    super(message, cause);
  }

  public TQLException(Throwable cause) {
    super(cause);
  }

  public TQLException(
      String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }
}
