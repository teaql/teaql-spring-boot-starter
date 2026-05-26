package io.teaql.data.utils;

@FunctionalInterface
public interface Filter<T> {
    boolean accept(T t);
}
