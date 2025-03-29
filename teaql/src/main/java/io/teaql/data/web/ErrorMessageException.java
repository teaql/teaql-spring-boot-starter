package io.teaql.data.web;

import io.teaql.data.TQLException;

public class ErrorMessageException extends TQLException {
    public ErrorMessageException() {
    }

    public ErrorMessageException(String message) {
        super(message);
    }

    public ErrorMessageException(String message, Throwable cause) {
        super(message, cause);
    }

    public ErrorMessageException(Throwable cause) {
        super(cause);
    }

    public ErrorMessageException(
            String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
