package java.io;

import com.facebook.infer.models.InferBuiltins;
import com.facebook.infer.models.InferUndefined;

public class PipedOutputStream extends OutputStream {

    private PipedInputStream target;

    public PipedOutputStream(PipedInputStream snk) throws IOException {
        this.target = snk;
        InferBuiltins.__set_file_attribute(target);
    }

    public PipedOutputStream() {
        this.target = new PipedInputStream();
        InferBuiltins.__set_file_attribute(target);
    }

    public void close() throws IOException {
        InferBuiltins.__set_mem_attribute(target);
        InferUndefined.can_throw_ioexception_void();
    }

    public void connect(PipedInputStream snk) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void flush() throws IOException {
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
}
