package java.net;

import java.io.*;
import java.util.Hashtable;

public class URLConnection {

    protected URL url;
    private String contentType;
    private static boolean defaultAllowUserInteraction;
    private static boolean defaultUseCaches;
    ContentHandler defaultHandler;
    private long lastModified;
    protected long ifModifiedSince;
    protected boolean useCaches;
    protected boolean connected;
    protected boolean doOutput;
    protected boolean doInput;
    protected boolean allowUserInteraction;

    private static ContentHandlerFactory contentHandlerFactory;
    private int readTimeout;
    private int connectTimeout;
    static Hashtable<String, Object> contentHandlers;
    private static FileNameMap fileNameMap;


    public URLConnection(URL url) {
        this.url = url;
    }

    public URL getURL() {
        return url;
    }

    public InputStream getInputStream() throws IOException {
        if (this instanceof HttpURLConnection) {
            byte[] arr = {1};
            return new ByteArrayInputStream(arr);
        } else return new FileInputStream("");
    }

    public OutputStream getOutputStream() throws IOException {
        if (this instanceof HttpURLConnection) {
            return new ByteArrayOutputStream();
        } else return new FileOutputStream("");
    }
}
