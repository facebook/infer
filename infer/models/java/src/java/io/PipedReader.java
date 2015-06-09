package java.io;

import com.facebook.infer.models.InferBuiltins;
import com.facebook.infer.models.InferUndefined;

public class PipedReader extends Reader {

    private Thread lastReader;

    private Thread lastWriter;

    private boolean isClosed;

    private void init() throws IOException {
        InferUndefined.can_throw_ioexception_void();
        this.lastReader = new Thread();
        InferBuiltins.__set_file_attribute(this.lastReader);
    }

    public PipedReader() {
    }

    public PipedReader(int pipeSize) {
    }

    public PipedReader(PipedWriter src) throws IOException {
        init();
    }

    public PipedReader(PipedWriter src, int pipeSize) throws IOException {
        init();
    }

    public void connect(PipedWriter src) throws IOException {
        init();
    }

    public int read() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(char[] cbuf, int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public boolean ready() throws IOException {
        return InferUndefined.can_throw_ioexception_boolean();
    }

    public void mark(int readAheadLimit) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void reset() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public long skip(long n) throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

    public void close() throws IOException {
        InferBuiltins.__set_mem_attribute(this.lastReader);
        InferUndefined.can_throw_ioexception_void();
    }

}
