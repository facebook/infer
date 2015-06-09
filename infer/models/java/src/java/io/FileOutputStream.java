package java.io;

import com.facebook.infer.models.InferBuiltins;
import com.facebook.infer.models.InferUndefined;
import dalvik.system.CloseGuard;

import java.nio.FileChannelImpl;
import java.nio.channels.FileChannel;

public class FileOutputStream extends OutputStream {

    private FileDescriptor fd;
    private boolean shouldClose;

    private FileChannel channel;

    private int mode;

    private CloseGuard guard;

    private void init() {
        this.guard = new CloseGuard();
        InferBuiltins.__set_file_attribute(this.guard);
    }

    public FileOutputStream(String name) throws FileNotFoundException {
        if (InferUndefined.boolean_undefined()) {
            init();
        } else {
            throw new FileNotFoundException();
        }
    }

    public FileOutputStream(String name, boolean append) throws FileNotFoundException {
        if (InferUndefined.boolean_undefined()) {
            init();
        } else {
            throw new FileNotFoundException();
        }
    }

    public FileOutputStream(File file) throws FileNotFoundException {
        if (InferUndefined.boolean_undefined()) {
            init();
        } else {
            throw new FileNotFoundException();
        }
    }

    public FileOutputStream(File file, boolean append)
            throws FileNotFoundException {
        if (InferUndefined.boolean_undefined()) {
            init();
        } else {
            throw new FileNotFoundException();
        }
    }

    public FileOutputStream(FileDescriptor fdObj) {
        init();
    }

    public FileChannel getChannel() {
        channel = new FileChannelImpl(this, fd, InferUndefined.int_undefined());
        return channel;
    }

    public void write(int b) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[]) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[], int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void close() throws IOException {
        InferBuiltins.__set_mem_attribute(this.guard);
        InferUndefined.can_throw_ioexception_void();
    }

}
