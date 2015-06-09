package java.security;

import com.facebook.infer.models.InferUndefined;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class DigestOutputStream extends FilterOutputStream {

    protected MessageDigest digest;
    private boolean isOn;

    public DigestOutputStream(OutputStream stream, MessageDigest digest) {
        super(stream);
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
}
