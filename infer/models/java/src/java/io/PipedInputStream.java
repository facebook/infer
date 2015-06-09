package java.io;

import com.facebook.infer.models.InferBuiltins;
import com.facebook.infer.models.InferUndefined;

public class PipedInputStream extends InputStream {

    private Thread lastReader;

    public PipedInputStream(PipedOutputStream src) throws IOException {
        this();
    }

    public PipedInputStream(PipedOutputStream src, int pipeSize)
            throws IOException {
        this();
    }

    public PipedInputStream() {
        lastReader = new Thread();
        InferBuiltins.__set_file_attribute(lastReader);
    }

    public PipedInputStream(int pipeSize) {
        this();
    }

    public void close() throws IOException {
        InferBuiltins.__set_mem_attribute(lastReader);
        InferUndefined.can_throw_ioexception_void();
    }

    public void connect(PipedOutputStream src) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public int available() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
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
