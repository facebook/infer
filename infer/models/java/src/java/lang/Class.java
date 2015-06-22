/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.lang;

public final class Class<T> {

    transient String name;

    public String getName() {
        return this.name;
    }

    public static Class<?> forName(String className)
            throws ClassNotFoundException {
        return new Class();
    }

    public boolean isAssignableFrom(Class<?> cls) {
        return false;
    }

    public static Class getPrimitiveClass(String name) {
        Class c = new Class();
        c.name = name;
        return c;
    }

}
