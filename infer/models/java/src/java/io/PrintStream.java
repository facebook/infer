package java.io;

public class PrintStream extends FilterOutputStream implements Closeable {

    private boolean ioError;
    private boolean autoFlush;
    private String encoding;

    public PrintStream(OutputStream out) {
        super(out);
    }

    public PrintStream(OutputStream out, boolean autoFlush) {
        super(out);
    }

    public PrintStream(OutputStream out, boolean autoFlush, String encoding)
            throws UnsupportedEncodingException {
        super(out);
    }

    public PrintStream(String fileName) throws FileNotFoundException {
        super(new FileOutputStream(fileName));
    }

    public PrintStream(String fileName, String csn)
            throws FileNotFoundException, UnsupportedEncodingException {
        super(new FileOutputStream(fileName));
    }

    public PrintStream(File file) throws FileNotFoundException {
        super(new FileOutputStream(file));
    }

    public PrintStream(File file, String csn) throws FileNotFoundException,
            UnsupportedEncodingException {
        super(new FileOutputStream(file));
    }

    public void close() {
        if (out != null) {
            try {
                if (out instanceof FileOutputStream)
                    ((FileOutputStream) out).close();
            } catch (IOException e) {
            }
        }
    }

    //the methods write and flush in this class do not throw IOExceptions, thus they are
    //modelled as a void method that does nothing.

    public void write(byte b[], int off, int len) {
    }

    public void write(byte b[]) throws IOException {
    }

    public void write(int b) {
    }

    public void flush() {
    }

}
