package java.util.zip;

import com.facebook.infer.models.InferUndefined;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class CheckedOutputStream extends FilterOutputStream {

    private Checksum check;

    public CheckedOutputStream(OutputStream out, Checksum cksum) {
        super(out);
    }

    public void write(int b) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[], int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[]) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }
}
