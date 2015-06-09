package java.io;

import com.facebook.infer.models.InferUndefined;

public class DataOutputStream extends FilterOutputStream {

    private byte[] scratch;
    protected int written;

    public DataOutputStream(OutputStream out) {
        super(out);
    }

    public void flush() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[], int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[]) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(int b) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeBoolean(boolean v) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeByte(int v) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeBytes(String s) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeChar(int v) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeChars(String s) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeDouble(double v) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeFloat(float v) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeInt(int v) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeLong(long v) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeShort(int v) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public final void writeUTF(String str) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

}
