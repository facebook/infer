Docker Image for FaceBook [infer](https://github.com/facebook/infer)


# Usage:

## Download infer image
`docker run -it doctorq/infer:v1.0 /bin/bash`
or
`docker run -it doctorq/infer:v1.1 /bin/bash`

## Go to infer dir

`cd infer`
# Analyze
> we can use examples in infer source code;


### Java file

```
root@cadacc19b8da:/infer/examples# infer -- javac Hello.java
Starting analysis (Infer version git-f6cb99fc551c982444baebb95cab0027d0c8c301)
Analysis done

1 file analyzed


/infer/examples/Hello.java:4: error: NULL_DEREFERENCE
  object s last assigned on line 3 could be null and is dereferenced at line 4

root@cadacc19b8da:/infer/examples# 
```

### Android project


```
root@f71395f22159:/infer/examples/android_hello# gradle clean
:app:clean

BUILD SUCCESSFUL

Total time: 6.522 secs

This build could be faster, please consider using the Gradle Daemon: http://gradle.org/docs/2.5/userguide/gradle_daemon.html
root@cadacc19b8da:/infer/examples/android_hello# infer -- gradle build
07:29:54.755 [ERROR] [org.gradle.api.Project] /infer/examples/android_hello/app/build/intermediates/exploded-aar/com.android.support/appcompat-v7/22.0.0/res/drawable-hdpi-v4/abc_spinner_mtrl_am_alpha.9.png: libpng warning: iCCP: Not recognizing known sRGB profile that has been edited
07:30:12.023 [ERROR] [org.gradle.api.Project] /infer/examples/android_hello/app/build/intermediates/exploded-aar/com.android.support/appcompat-v7/22.0.0/res/drawable-hdpi-v4/abc_spinner_mtrl_am_alpha.9.png: libpng warning: iCCP: Not recognizing known sRGB profile that has been edited
Starting analysis (Infer version git-f6cb99fc551c982444baebb95cab0027d0c8c301)
Analysis done

5 files analyzed


/infer/examples/android_hello/app/src/main/java/infer/inferandroidexample/MainActivity.java:20: error: NULL_DEREFERENCE
  object s last assigned on line 19 could be null and is dereferenced at line 20

/infer/examples/android_hello/app/src/main/java/infer/inferandroidexample/MainActivity.java:37: error: RESOURCE_LEAK
   resource acquired to fis by call to FileOutputStream(...) at line 34 is not released after line 37

```

### C file

```
root@f71395f22159:/infer/examples# infer -- gcc -c hello.c
Starting analysis (Infer version git-1356fd331f3db485be9ee38446a7389c9310d344)
Analysis done

1 file analyzed


hello.c:5: error: NULL_DEREFERENCE
  pointer s last assigned on line 4 could be null and is dereferenced at line 5, column 3

```

### C project

```

root@f71395f22159:/infer/examples/c_hello# make clean
rm -rf example.o
root@f71395f22159:/infer/examples/c_hello# infer -- make
cc -c example.c
Starting analysis (Infer version git-1356fd331f3db485be9ee38446a7389c9310d344)
Analysis done

1 file analyzed


example.c:22: error: NULL_DEREFERENCE
  pointer max last assigned on line 21 could be null and is dereferenced at line 22, column 10

example.c:36: error: NULL_DEREFERENCE
  pointer joe last assigned on line 35 could be null and is dereferenced by call to get_age() at line 36, column 10

example.c:45: error: RESOURCE_LEAK
   resource acquired to fd by call to open() at line 41, column 12 is not released after line 45, column 5

example.c:51: error: MEMORY_LEAK
   memory dynamically allocated to p by call to malloc() at line 51, column 14 is not reachable after line 51, column 3

example.c:57: error: MEMORY_LEAK
   memory dynamically allocated to p by call to malloc() at line 56, column 14 is not reachable after line 57, column 3

 

```

#Docker hub

[doctorq/infer](https://registry.hub.docker.com/u/doctorq/infer/)



