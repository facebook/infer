package java.io;

import com.facebook.infer.models.InferUndefined;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class ObjectInputStream extends InputStream {

    private InputStream emptyStream;
    private static Object UNSHARED_OBJ;
    private boolean hasPushbackTC;
    private byte pushbackTC;
    private int nestedLevels;
    private int nextHandle;
    private DataInputStream input;

    private DataInputStream primitiveTypes;
    private InputStream primitiveData;
    private boolean enableResolve;
    private ArrayList<Object> objectsRead;
    private Object currentObject;
    private ObjectStreamClass currentClass;
    private InputValidationDesc[] validations;

    private boolean subclassOverridingImplementation;
    private ClassLoader callerClassLoader;
    private boolean mustResolve;
    private int descriptorHandle;

    private static HashMap<String, Class<?>> PRIMITIVE_CLASSES;

    static class InputValidationDesc {
        ObjectInputValidation validator;
        int priority;
    }

    private static ClassLoader bootstrapLoader;
    private static ClassLoader systemLoader;

    private HashMap<Class<?>, List<Class<?>>> cachedSuperclasses;

    public ObjectInputStream(InputStream in) throws IOException {
        this.input = new DataInputStream(in);
        InferUndefined.can_throw_ioexception_void();
    }

    protected ObjectInputStream() throws IOException, SecurityException {
    }

    public int available() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public void close() throws IOException {
        input.close();
    }

    public void defaultReadObject() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public int read() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(byte b[]) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(byte b[], int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public boolean readBoolean() throws IOException {
        return InferUndefined.can_throw_ioexception_boolean();
    }

    public byte readByte() throws IOException {
        return InferUndefined.can_throw_ioexception_byte();
    }

    public char readChar() throws IOException {
        return InferUndefined.can_throw_ioexception_char();
    }

    public double readDouble() throws IOException {
        return InferUndefined.can_throw_ioexception_double();
    }

    public ObjectInputStream.GetField readFields() throws IOException {
        throw new IOException();
    }

    public float readFloat() throws IOException {
        return InferUndefined.can_throw_ioexception_float();
    }

    public void readFully(byte[] buf) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void readFully(byte[] buf, int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public int readInt() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public long readLong() throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

    public final Object readObject() throws IOException {
        return InferUndefined.can_throw_ioexception_object();
    }

    public short readShort() throws IOException {
        return InferUndefined.can_throw_ioexception_short();
    }

    public Object readUnshared() throws IOException, ClassNotFoundException {
        return InferUndefined.can_throw_ioexception_object();
    }

    public int readUnsignedByte() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int readUnsignedShort() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public String readUTF() throws IOException {
        return InferUndefined.can_throw_ioexception_string();
    }

    public int skipBytes(int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public static abstract class GetField {
    }
}
