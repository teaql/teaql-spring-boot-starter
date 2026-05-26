package io.teaql.data.utils;

public class TimedCache<K, V> implements Cache<K, V> {
    private final cn.hutool.cache.impl.TimedCache<K, V> delegate;

    public TimedCache(long timeout) {
        this.delegate = new cn.hutool.cache.impl.TimedCache<>(timeout);
    }

    @Override
    public void put(K key, V value) {
        delegate.put(key, value);
    }

    @Override
    public void put(K key, V value, long timeout) {
        delegate.put(key, value, timeout);
    }

    @Override
    public V get(K key) {
        return delegate.get(key);
    }

    @Override
    public V get(K key, boolean isUpdate) {
        return delegate.get(key, isUpdate);
    }

    @Override
    public V get(K key, java.util.function.Supplier<? extends V> supplier) {
        return delegate.get(key, () -> supplier.get());
    }

    @Override
    public void remove(K key) {
        delegate.remove(key);
    }

    @Override
    public boolean containsKey(K key) {
        return delegate.containsKey(key);
    }
}
