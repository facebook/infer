package java.util.jar;

import com.facebook.infer.models.InferUndefined;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.zip.ZipInputStream;

public class JarInputStream extends ZipInputStream {

    private Manifest manifest;

    private boolean eos;

    private JarEntry mEntry;

    private JarEntry jarEntry;

    private boolean isMeta;

    private JarVerifier verifier;

    private OutputStream verStream;

    public JarInputStream(InputStream in) throws IOException {
        super(in);
        InferUndefined.can_throw_ioexception_void();
    }

    public JarInputStream(InputStream in, boolean verify) throws IOException {
        super(in);
        InferUndefined.can_throw_ioexception_void();
    }

}
