package java.security;

import com.facebook.infer.models.InferUndefined;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

public class DigestInputStream extends FilterInputStream {

    protected MessageDigest digest;

    private boolean isOn;

    public DigestInputStream(InputStream stream, MessageDigest digest) {
        super(stream);
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
}
