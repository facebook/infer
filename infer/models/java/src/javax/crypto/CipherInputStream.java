package javax.crypto;

import com.facebook.infer.models.InferUndefined;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

public class CipherInputStream extends FilterInputStream {

    private static int I_BUFFER_SIZE;

    private Cipher cipher;
    private byte[] inputBuffer;
    private byte[] outputBuffer;
    private int outputIndex;
    private int outputLength;
    private boolean finished;

    public CipherInputStream(InputStream is, Cipher c) {
        super(is);
    }

    protected CipherInputStream(InputStream is) {
        super(is);
    }

    public int available() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public void close() throws IOException {
        super.close();
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

    public long skip(long n) throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

}
