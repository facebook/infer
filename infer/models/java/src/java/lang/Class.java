package java.lang;

public final class Class<T> {
    private static long serialVersionUID;

    private transient int dexClassDefIndex;

    private transient int dexTypeIndex;

    private transient volatile boolean dexIndicesInitialized;

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
