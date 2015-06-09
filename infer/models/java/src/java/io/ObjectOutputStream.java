package java.io;

import com.facebook.infer.models.InferUndefined;

public class ObjectOutputStream extends OutputStream {

    private static Class<?>[] WRITE_UNSHARED_PARAM_TYPES;
    private static byte NOT_SC_BLOCK_DATA;
    private int nestedLevels;
    private DataOutputStream output;
    private boolean enableReplace;
    private DataOutputStream primitiveTypes;
    private ByteArrayOutputStream primitiveTypesBuffer;
    private SerializationHandleMap objectsWritten;
    private int currentHandle;
    private Object currentObject;
    private ObjectStreamClass currentClass;
    private int protocolVersion;
    private StreamCorruptedException nestedException;
    private EmulatedFieldsForDumping currentPutField;
    private boolean subclassOverridingImplementation;
    private ObjectStreamClass proxyClassDesc;

    public ObjectOutputStream(OutputStream out) throws IOException {
        this.output = new DataOutputStream(out);
        InferUndefined.can_throw_ioexception_void();
    }

    public void close() throws IOException {
        output.close();
    }

    public void defaultWriteObject() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void flush() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void reset() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[]) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[], int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(int b) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeBoolean(boolean val) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeByte(int val) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeBytes(String str) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeChar(int val) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeChars(String str) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeDouble(double val) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeFields() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeFloat(float val) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeInt(int val) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeLong(long val) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeObject(Object obj) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeShort(int val) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeUnshared(Object obj) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void writeUTF(String str) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }
}
