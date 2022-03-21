/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import android.app.Activity;
import android.content.Context;
import android.content.res.Resources;
import android.content.res.TypedArray;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.json.UTF8StreamJsonParser;
import com.google.common.base.Preconditions;
import com.google.common.io.Closeables;
import java.io.BufferedInputStream;
import java.io.Closeable;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.net.HttpURLConnection;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URL;
import java.net.URLConnection;
import java.nio.channels.FileChannel;
import java.util.Scanner;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;
import java.util.jar.JarOutputStream;
import java.util.zip.Deflater;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.zip.Inflater;
import java.util.zip.ZipFile;
import javax.net.ssl.HttpsURLConnection;

public class ResourceLeaks {

  // FileOutputStream tests

  public void fileOutputStreamNotClosed() throws IOException {
    FileOutputStream fis = new FileOutputStream("file.txt");
  }

  public void fileOutputStreamNotClosedAfterWrite() {
    byte[] arr = {1, 2, 3};
    FileOutputStream fis = null;
    try {
      fis = new FileOutputStream("file.txt");
      fis.write(arr);
      fis.close();
    } catch (IOException e) {
    }
  }

  public void fileOutputStreamClosed() throws IOException {
    FileOutputStream fis = new FileOutputStream("file.txt");
    fis.close();
  }

  public void fileOutputStreamOneLeak() throws IOException {
    FileOutputStream fis = new FileOutputStream("file.txt");
    if (fis != null) {
    } else {
    }
  }

  public int fileOutputStreamTwoLeaks1(boolean ok) throws IOException {
    FileOutputStream fis = new FileOutputStream("file.txt");
    if (ok) {
      fis.write(1);
      return 1;
    } else {
      fis.write(2);
      return 2;
    }
  }

  public void fileOutputStreamTwoLeaks2() throws IOException {
    FileOutputStream fis = new FileOutputStream("file.txt");
    if (fis != null) {
    } else {
    }
    fis = new FileOutputStream("x");
  }

  // TwoResources tests

  public static void twoResources() throws IOException {
    FileInputStream fis = null;
    FileOutputStream fos = null;
    try {
      fis = new FileInputStream(new File("infile.txt"));
      fos = new FileOutputStream(new File("outfile.txt"));
      fos.write(fis.read());
    } finally {
      if (fis != null) fis.close();
      if (fos != null) fos.close();
    }
  }

  public static void twoResourcesHeliosFix() throws IOException {
    FileInputStream fis = null;
    FileOutputStream fos = null;
    try {
      fis = new FileInputStream(new File("whatever.txt"));
      try {
        fos = new FileOutputStream(new File("everwhat.txt"));
        fos.write(fis.read());
      } finally {
        if (fos != null) fos.close();
      }
    } finally {
      if (fis != null) fis.close();
    }
  }

  public static void twoResourcesCommonFix() throws IOException {
    FileInputStream fis = null;
    FileOutputStream fos = null;
    try {
      fis = new FileInputStream(new File("infile.txt"));
      fos = new FileOutputStream(new File("outfile.txt"));
      fos.write(fis.read());
    } finally {
      try {
        if (fis != null) fis.close();
      } catch (Exception e) {
      }
      try {
        if (fos != null) fos.close();
      } catch (Exception e) {
      }
    }
  }

  public static void twoResourcesServerSocket() throws IOException {
    ServerSocket a = null;
    ServerSocket b = null;
    try {
      a = new ServerSocket();
      b = new ServerSocket();
    } finally {
      if (a != null) a.close();
      if (b != null) b.close();
    }
  }

  public static void twoResourcesRandomAccessFile() throws IOException {
    RandomAccessFile a = null;
    RandomAccessFile b = null;
    try {
      a = new RandomAccessFile("", "rw");
      b = new RandomAccessFile("", "rw");
    } finally {
      if (a != null) a.close();
      if (b != null) b.close();
    }
  }

  public static void twoResourcesRandomAccessFileCommonFix() throws IOException {
    RandomAccessFile a = null;
    RandomAccessFile b = null;
    try {
      a = new RandomAccessFile("", "rw");
      b = new RandomAccessFile("", "rw");
    } finally {
      try {
        if (a != null) a.close();
      } catch (Exception e) {
      }
      try {
        if (b != null) b.close();
      } catch (Exception e) {
      }
    }
  }

  // NestedResource tests

  // BufferedInputStream does not throw exception, and its close
  // closes the FileInputStream as well
  public void nestedGood() throws IOException {
    BufferedInputStream b = new BufferedInputStream(new FileInputStream("file.txt"));
    b.close();
  }

  // GZipInputStream can throw IO Exception
  // in which case the new FileInputStream will be dangling
  public void nestedBad1() throws IOException {
    GZIPInputStream g = new GZIPInputStream(new FileInputStream("file.txt"));
    g.close();
  }

  public void nestedBad2() throws IOException {
    GZIPOutputStream g = new GZIPOutputStream(new FileOutputStream("file.txt"));
    g.close();
  }

  /* Fixed versions of this are below with ObjectInputStream tests */
  public void objectInputStreamClosedNestedBad() throws IOException {
    ObjectInputStream oin = null;
    try {
      oin = new ObjectInputStream(new FileInputStream("file.txt"));
      int a = oin.available();
    } catch (IOException e) {
    } finally {
      if (oin != null) oin.close();
    }
  }

  /* Fixed versions of this are below with ObjectInputStream tests */
  public void objectOutputStreamClosedNestedBad() throws IOException {
    ObjectOutputStream oin = null;
    try {
      oin = new ObjectOutputStream(new FileOutputStream("file.txt"));
      oin.write(3);
    } catch (IOException e) {
    } finally {
      if (oin != null) oin.close();
    }
  }

  // ZipFile tests      (Jarfile Tests also test Zipfiles)

  public static void zipFileLeakExceptionalBranch() throws IOException {
    ZipFile j = null;
    try {
      j = new ZipFile("");
    } catch (IOException e) {
      FileOutputStream fis = new FileOutputStream("file.txt");
      // The purpose of this is to cause a leak, from when ZipFile constructor throws
    } finally {
      if (j != null) j.close();
    }
  }

  public static void zipFileNoLeak() throws IOException {
    ZipFile j = null;
    try {
      j = new ZipFile("");
    } finally {
      if (j != null) j.close();
    }
  }

  // JarFile tests

  public boolean jarFileClosed() {
    JarFile jarFile = null;
    try {
      jarFile = new JarFile("");
    } catch (IOException e) {
    } finally {
      try {
        if (jarFile != null) {
          jarFile.close();
        }
      } catch (IOException e) {
      }
    }
    return false;
  }

  public boolean jarFileNotClosed() {
    JarFile jarFile = null;
    try {
      jarFile = new JarFile("");
    } catch (IOException e) {
    }
    return false;
  }

  // FileInputStream tests

  public void fileInputStreamNotClosedAfterRead() {
    FileInputStream fis;
    try {
      fis = new FileInputStream("file.txt");
      fis.read();
      fis.close();
    } catch (IOException e) {
    }
  }

  public void fileInputStreamClosed() throws IOException {
    FileInputStream fis = null;
    try {
      fis = new FileInputStream("file.txt");
      fis.available();
    } catch (IOException e) {
    } finally {
      if (fis != null) fis.close();
    }
  }

  // PipedInputStream tests

  public void pipedInputStreamNotClosedAfterRead(PipedOutputStream pout) {
    PipedInputStream pin;
    try {
      pin = new PipedInputStream(pout);
      int data = pin.read();
      pin.close();
    } catch (IOException e) {
    }
  }

  public void pipedInputStreamClosed(PipedOutputStream pout) throws IOException {
    PipedInputStream pin = null;
    try {
      pin = new PipedInputStream(pout);
      int data = pin.read();
    } catch (IOException e) {
    } finally {
      pin.close();
    }
  }

  // PipedOutputStream tests

  public void pipedOutputStreamNotClosedAfterWrite() {
    byte[] arr = {1, 2, 3, 4, 5};
    PipedOutputStream pout;
    try {
      pout = new PipedOutputStream();
      pout.write(arr);
      pout.close();
    } catch (IOException e) {
    }
  }

  public void pipedOutputStreamClosed(PipedInputStream pin) throws IOException {
    PipedOutputStream pout = null;
    try {
      pout = new PipedOutputStream(pin);
      pout.flush();
    } catch (IOException e) {
    } finally {
      pout.close();
    }
  }

  // ObjectOutputStream tests

  public void objectOutputStreamNotClosedAfterWrite() {
    byte[] arr = {1, 2, 3, 4, 5};
    ObjectOutputStream oout;
    try {
      oout = new ObjectOutputStream(new FileOutputStream("file.txt"));
      oout.write(arr);
      oout.close();
    } catch (IOException e) {
    }
  }

  public void objectOutputStreamClosed() throws IOException {
    ObjectOutputStream oout = null;
    FileOutputStream fis = new FileOutputStream("file.txt");
    try {
      oout = new ObjectOutputStream(fis);
      oout.flush();
    } catch (IOException e) {
    } finally {
      fis.close();
    }
  }

  // ObjectInputStream tests

  public void objectInputStreamNotClosedAfterRead() {
    ObjectInputStream oin;
    try {
      oin = new ObjectInputStream(new FileInputStream("file.txt"));
      oin.read();
      oin.close();
    } catch (IOException e) {
    }
  }

  public void objectInputStreamClosed() throws IOException {
    ObjectInputStream oin = null;
    FileInputStream fis = new FileInputStream("file.txt");
    try {
      oin = new ObjectInputStream(fis);
      int a = oin.available();
    } catch (IOException e) {
    } finally {
      if (oin != null) {
        oin.close();
      } else {
        fis.close();
      }
    }
  }

  public void objectInputStreamClosed2() throws IOException {
    ObjectInputStream oin = null;
    FileInputStream fis = new FileInputStream("file.txt");
    try {
      oin = new ObjectInputStream(fis);
      int a = oin.available();
    } catch (IOException e) {
    } finally {
      fis.close();
    }
  }

  // JarInputStream tests

  public static void jarInputStreamNoLeak() throws IOException {
    FileInputStream fos = new FileInputStream("");
    try {
      JarInputStream g = new JarInputStream(fos);
      g.close();
    } catch (IOException e) {
      fos.close();
    }
  }

  public static void jarInputStreamLeak() throws IOException {
    FileInputStream fos = new FileInputStream("");
    try {
      JarInputStream g = new JarInputStream(fos); //  Testing exceptional condition in constructor
      g.close();
    } catch (IOException e) {
      // fos.close();
    }
  }

  public static void nestedBadJarInputStream(File file) throws IOException {
    JarInputStream g = new JarInputStream(new FileInputStream(file));
    g.close();
  }

  // JarOutputStream tests

  public static void jarOutputStreamNoLeak() throws IOException {
    FileOutputStream fos = new FileOutputStream("");
    try {
      JarOutputStream g = new JarOutputStream(fos);
      g.close();
    } catch (IOException e) {
      fos.close();
    }
  }

  public static void jarOutputStreamLeak() throws IOException {
    FileOutputStream fos = new FileOutputStream("");
    try {
      JarOutputStream g = new JarOutputStream(fos); //  Testing exceptional condition in constructor
      g.close();
    } catch (IOException e) {
      // fos.close();
    }
  }

  public static void nestedBadJarOutputStream() throws IOException {
    JarOutputStream g = new JarOutputStream(new FileOutputStream("file.txt"));
    g.close();
  }

  // Socket tests

  public void socketNotClosed() {
    Socket socket = new Socket();
  }

  public void socketClosed() throws IOException {
    Socket socket = new Socket();
    socket.close();
  }

  // Socket InputStream tests

  public int socketInputStreamNotClosed(Socket socket) throws IOException {
    InputStream stream = socket.getInputStream();
    return stream.read();
  }

  public void socketInputStreamClosed() throws IOException {
    Socket socket = new Socket();
    InputStream stream = socket.getInputStream();
    try {
      stream.close();
    } catch (Exception e) {
    }
    socket.close();
  }

  // Socket OutputStream tests

  public void socketOutputStreamNotClosed(Socket socket) throws IOException {
    OutputStream stream = socket.getOutputStream();
    stream.write(10);
  }

  public void socketOutputStreamClosed() throws IOException {
    Socket socket = new Socket();
    OutputStream stream = socket.getOutputStream();
    try {
      stream.close();
    } catch (Exception e) {
    }
    socket.close();
  }

  // ServerSocket tests

  public void serverSocketNotClosed() throws IOException {
    ServerSocket listener = new ServerSocket(9090);
    while (true) {
      Socket socket = listener.accept();
      try {
        PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
        out.println("");
      } finally {
        socket.close();
      }
      listener.close();
    }
  }

  public void serverSocketClosed() throws IOException {
    ServerSocket socket = new ServerSocket();
    socket.close();
  }

  public void serverSocketWithSocketClosed() throws IOException {
    ServerSocket listener = new ServerSocket(9090);
    try {
      while (true) {
        Socket socket = listener.accept();
        try {
          PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
          out.println("");
        } finally {
          socket.close();
        }
      }
    } finally {
      listener.close();
    }
  }

  // HttpURLConnection

  public void openHttpURLConnectionDisconnected() throws IOException {
    String content = "TEXT";
    DataOutputStream outputStream = null;
    HttpURLConnection connection = null;
    URL address = new URL("http://www.facebook.com");
    connection = (HttpURLConnection) address.openConnection();
    try {
      outputStream = new DataOutputStream(connection.getOutputStream());
      outputStream.writeBytes(content);
      outputStream.flush();

    } finally {
      connection.disconnect();
    }
  }

  public void openHttpURLConnectionNotDisconnected() throws IOException {
    String content = "TEXT";
    DataOutputStream outputStream = null;
    HttpURLConnection connection = null;
    URL address = new URL("http://www.facebook.com");
    connection = (HttpURLConnection) address.openConnection();

    outputStream = new DataOutputStream(connection.getOutputStream());
    outputStream.writeBytes(content);
  }

  public void openHttpsURLConnectionNotDisconnected() throws IOException {
    HttpsURLConnection connection = null;
    URL address = new URL("https://www.facebook.com");
    connection = (HttpsURLConnection) address.openConnection();
  }

  public void openHttpsURLConnectionDisconnected() throws IOException {
    HttpsURLConnection connection = null;
    URL address = new URL("https://www.facebook.com");
    connection = (HttpsURLConnection) address.openConnection();
    connection.disconnect();
  }

  public void closedWithCloseables() throws IOException {
    FileInputStream fs = new FileInputStream("file.txt");
    try {
      fs.read();
    } finally {
      Closeables.close(fs, false);
    }
  }

  public void closedQuietlyWithCloseables() throws IOException {
    FileInputStream fs = new FileInputStream("file.txt");
    try {
      fs.read();
    } finally {
      Closeables.closeQuietly(fs);
    }
  }

  public void closeNullWithCloseables() throws IOException {
    FileInputStream fs = null;
    try {
      fs = new FileInputStream("file.txt");
    } finally {
      Closeables.close(fs, true);
    }
  }

  public void closeNullQuietlyWithCloseables() throws IOException {
    FileInputStream fs = null;
    try {
      fs = new FileInputStream("file.txt");
    } finally {
      Closeables.closeQuietly(fs);
    }
  }

  private static void myClose(Closeable closeable, boolean swallowIOException) throws IOException {
    if (closeable == null) {
      return;
    }
    try {
      closeable.close();
    } catch (IOException e) {
      if (!swallowIOException) {
        throw e;
      }
    }
  }

  public void closeWithCloseablesNestedAlloc() throws IOException {
    BufferedInputStream b = null;
    try {
      b = new BufferedInputStream(new FileInputStream("file.txt"));
    } finally {
      myClose(b, false);
    }
  }

  // JsonParser tests

  public void parseFromStringAndNotClose(JsonFactory factory) throws IOException {
    UTF8StreamJsonParser parser = null;
    try {
      parser = (UTF8StreamJsonParser) factory.createParser(new File("[]"));
      Object o = parser.readValueAs(Object.class);
      ignore(o);
    } catch (Exception e) {
    } finally {
    }
  }

  public void parseFromInputStreamAndClose(JsonFactory factory) throws IOException {
    JsonParser parser = null;
    FileInputStream in = null;
    try {
      in = new FileInputStream("");
      parser = factory.createParser(in);
      Object o = parser.readValueAs(Object.class);
      ignore(o);
    } catch (Exception e) {
    } finally {
      if (in != null) in.close();
    }
    // parser does not own a resources which is closed externally
  }

  public void parseFromInputStreamAndLeak(JsonFactory factory) throws IOException {
    JsonParser parser = null;
    FileInputStream in = null;
    try {
      in = new FileInputStream("");
      parser = factory.createParser(in);
      Object o = parser.readValueAs(Object.class);
      ignore(o);
    } catch (Exception e) {
    } finally {
      if (parser != null) parser.close();
    }
    // parser does not own a resource which is leaked
  }

  private void ignore(Object o) {}

  // Installation.java examples. Even the fix was a fp for a while
  // for several reasons, so this test is just to make sure it remains
  // banished forever

  private String readInstallationFileGood(File installation) throws IOException {
    RandomAccessFile f = new RandomAccessFile(installation, "r");
    try {
      byte[] bytes = new byte[(int) f.length()];
      f.readFully(bytes);
      return new String(bytes);
    } finally {
      f.close();
    }
  }

  private String readInstallationFileBad(File installation) throws IOException {
    RandomAccessFile f = new RandomAccessFile(installation, "r");
    byte[] bytes = new byte[(int) f.length()];
    f.readFully(bytes);
    f.close();
    return new String(bytes);
  }

  private int readConfigCloseStream(String mTurnConfigUrl) {
    try {
      URL url = new URL(mTurnConfigUrl);
      URLConnection connection = url.openConnection();
      InputStream stream = connection.getInputStream();
      try {
        return stream.read();
      } finally {
        stream.close();
      }
    } catch (Exception e) {
    }
    return 0;
  }

  private int readConfigNotCloseStream(String mTurnConfigUrl) {
    try {
      URL url = new URL(mTurnConfigUrl);
      URLConnection connection = url.openConnection();
      InputStream stream = connection.getInputStream();
      return stream.read();
    } catch (Exception e) {
    }
    return 0;
  }

  private void readConfigNotClosedOK(String mTurnConfigUrl) {
    try {
      URL url = new URL(mTurnConfigUrl);
      URLConnection connection = url.openConnection();
      ignore(connection);
    } catch (Exception e) {
    }
  }

  // TypedArray

  public void themeObtainTypedArrayAndRecycle(Resources.Theme theme) {
    TypedArray array = theme.obtainStyledAttributes(new int[] {});
    ignore(array);
    array.recycle();
  }

  public void themeObtainTypedArrayAndLeak(Resources.Theme theme) {
    TypedArray array = theme.obtainStyledAttributes(new int[] {});
    ignore(array);
  }

  public void activityObtainTypedArrayAndRecycle(Activity activity) {
    TypedArray array = activity.obtainStyledAttributes(new int[] {});
    ignore(array);
    array.recycle();
  }

  public void activityObtainTypedArrayAndLeak(Activity activity) {
    TypedArray array = activity.obtainStyledAttributes(new int[] {});
    ignore(array);
  }

  public void contextObtainTypedArrayAndRecycle(Context context) {
    TypedArray array = context.obtainStyledAttributes(new int[] {});
    ignore(array);
    array.recycle();
  }

  public void contextObtainTypedArrayAndLeak(Context context) {
    TypedArray array = context.obtainStyledAttributes(new int[] {});
    ignore(array);
  }

  // FileChannel

  void copyFileLeak(File src, File dst) throws IOException {
    FileChannel inChannel = new FileInputStream(src).getChannel();
    FileChannel outChannel = new FileOutputStream(dst).getChannel();
    try {
      inChannel.transferTo(0, inChannel.size(), outChannel);
    } finally {
      if (inChannel != null) inChannel.close();
      if (outChannel != null) outChannel.close();
    }
  }

  void copyFileClose(File src, File dst) throws IOException {
    FileChannel inChannel = new FileInputStream(src).getChannel();
    try {
      ignore(inChannel);
    } finally {
      inChannel.close();
    }
  }

  protected long checkNotNullCauseNoLeak(URL mUrl) throws IOException {
    URL url = new URL("http://www.facebook.com");
    HttpURLConnection serverConnection =
        (HttpURLConnection) Preconditions.checkNotNull(url.openConnection());
    try {
      ignore(serverConnection);
    } catch (NumberFormatException nfe) {
    } finally {
      serverConnection.disconnect();
    }
    return 0;
  }

  void scannerNotClosed() throws IOException {
    Scanner scanner = new Scanner(new FileInputStream("file.txt"));
  }

  void scannerClosed() throws IOException {
    Scanner scanner = new Scanner(new FileInputStream("file.txt"));
    scanner.close();
  }

  void processDestroyed() {
    Process process = null;
    try {
      process = Runtime.getRuntime().exec("");
    } catch (IOException e) {
    } finally {
      process.destroy();
    }
  }

  void processForciblyDestroyed() throws IOException {
    Process process = null;
    try {
      process = Runtime.getRuntime().exec("");
    } finally {
      ignore(process.destroyForcibly());
    }
  }

  class Container {
    FileInputStream inputStream;
  }

  native Container load(FileInputStream inputStream);

  public Container resourceReturnedIndirectly() {
    FileInputStream inputStream;
    Container container = null;
    try {
      inputStream = new FileInputStream("pif.txt");
      container = load(inputStream);
    } catch (FileNotFoundException e) {
      return null;
    }
    return container;
  }

  native void unknownClose(Closeable c);

  public void resourceClosedBySkippedMethod() {
    FileInputStream inputStream = null;
    try {
      inputStream = new FileInputStream("pif.txt");
    } catch (FileNotFoundException e) {
      return;
    } finally {
      unknownClose(inputStream);
    }
  }

  /* This program is not compiled the same way in Java 8 and Java 11
     but it has currently a dangling pointer FP. Since the report differs
     in version 8 and 11, we must remove it to please the CI.

  public int FP_tryWithResource() {
    try (FileInputStream inputStream = new FileInputStream("paf.txt")) {
      return inputStream.read();
    } catch (IOException e) {
      return 0;
    }
  }
  */

  public InputStreamReader withCharset(URLConnection urlConnection) {
    InputStreamReader reader = null;
    try {
      reader = new InputStreamReader(urlConnection.getInputStream(), "iso-8859-1");
    } catch (Exception e) {
      return null;
    } finally {
      if (reader != null) {
        try {
          reader.close();
        } catch (IOException e) {
          // do nothing
        }
      }
    }
    return reader;
  }

  public void withZipFile() throws IOException {
    ZipFile f = new ZipFile("hi");
    InputStream s = f.getInputStream(f.getEntry("there"));
    if (s != null) s.toString();
    f.close();
  }

  public void deflaterLeak() {
    Deflater comp = new Deflater();
  }

  public void deflaternoLeak() {
    Deflater comp = new Deflater();
    comp.end();
  }

  public void inflaterLeak() {
    Inflater decomp = new Inflater();
  }

  public void inflaterNoLeak() {
    Inflater decomp = new Inflater();
    decomp.end();
  }

  void NoResourceLeakWarningAfterCheckState(File f, int x) throws IOException {
    InputStream stream = new FileInputStream(f);
    Preconditions.checkState(x > 0);
    stream.close();
  }
}
