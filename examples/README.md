This directory contains small examples to play with Infer. They each exhibit
one simple programming error that is caught by Infer.

Contents
--------

- `Hello.java`: try this example by running
```infer -- javac Hello.java ```

- `Hello.m`: try this example by running
  ```infer -- clang -c Hello.m```

- `hello.c`: try this example by running
  ```infer -- gcc -c hello.c```

  In this case, note that Infer captures the gcc command and runs
  clang instead to parse C files. Thus you may get compiler errors and
  warnings that differ from gcc's.

- `android_hello/`: a sample Android app. Try this example by running
  ```infer -- ./gradlew build```

  Make sure that you have the Android SDK 22 installed and up to date, and in
  particular the "Android SDK Build-tools" and "Android Support Repository".

- `c_hello/`: a sample make-based C project. Try this example by running
  ```infer -- make```

- `ios_hello/`: a sample iOS app. Try this example by running
  ```infer -- xcodebuild -target HelloWorldApp -configuration Debug -sdk iphonesimulator```

- `java_hello/`: a sample Java project. Try this example by running
  ```infer -- javac Pointers.java Resources.java Hello.java```

Note
----

The infer toplevel command must be in your PATH for the commands above to
succeed. Otherwise, modify the commands to use the correct path to infer, eg
  ```../infer/bin/infer -- javac Hello.java```

