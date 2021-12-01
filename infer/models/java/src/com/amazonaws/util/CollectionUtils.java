package com.amazonaws.util;

import java.util.Collection;

public class CollectionUtils {

    public static <T> boolean isNullOrEmpty(Collection<T> collection) {
        return collection == null || collection.isEmpty();
    }
}
