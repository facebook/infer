package java.net;

import com.facebook.infer.models.InferBuiltins;

public class HttpURLConnection extends URLConnection {

    public HttpURLConnection(URL url) {
        super(url);
        InferBuiltins.__set_file_attribute(this);
    }

    public void disconnect() {
        InferBuiltins.__set_mem_attribute(this);
    }
}
