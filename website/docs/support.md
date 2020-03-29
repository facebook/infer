---
title: Infer | Need help?
id: support
---

## Need help?

Do not hesitate to ask questions using the following channels, or to submit pull
request!

### GitHub issues

The [GitHub issues](https://github.com/facebook/Infer/issues) page is a good
place to ask questions, find answers, and report issues.

Please include as many details as possible when submitting a GitHub issue. If
your are able to run Infer, please include the contents of
`infer-out/toplevel.log` in your report. If not, please include at least your
operating system and the version of Infer that you are using.

### Updates

Keep up to date with the latest Infer news on our
[Facebook page](https://www.facebook.com/inferstaticanalyzer/) and our
[Twitter account](https://twitter.com/fbinfer).

### IRC

Our IRC channel is [#infer](irc://chat.freenode.net/infer) on Freenode.net.

## Troubleshooting

### Infer cannot analyze my CocoaPods project

In the presence of CocoaPods, you should use xcworkspace and not xcodeproj in
the compilation command that you supply to Infer. Here is an example you can
adapt to your project:

```bash
infer run -- xcodebuild -workspace HelloWorld.xcworkspace -scheme HelloWorld
```

### "infer [options] -- \<build command\>" fails during a linking step

The linker will sometimes not work if files have been compiled using a different
compiler, such as the one Infer uses [under the hood](/docs/infer-workflow) to
analyze your files.

A workaround consists in setting the `LD` environment variable to a dummy
linker, for instance:

```
LD=/bin/true infer [options] -- <build command>
```

### I get a compilation error involving PCH files when running Infer

For instance,
`error: PCH file uses an older PCH format that is no longer supported`.

This is a [known issue](https://github.com/facebook/infer/issues/96).

Please run Infer with the following environment variable setting:

```bash
GCC_PRECOMPILE_PREFIX_HEADER=NO
```

### Using Infer with Maven results in no output

Try upgrading `maven-compiler-plugin`. See also
[this GitHub issue](https://github.com/facebook/infer/issues/38).

### Infer reports a "Too many open files" error

The maximum number of files a program can simultaneously hold open is a bit low
on MacOs. You can increase the limit by running these commands for example:

```bash
sysctl -w kern.maxfiles=20480
sysctl -w kern.maxfilesperproc=22480
ulimit -S -n 2048
```

Note that the settings will be reset at the next reboot.

See also [this GitHub issue](https://github.com/facebook/infer/issues/22).

### I get a lint error when running Infer with gradle

You need to manually disable linters to run Infer. For instance

```bash
infer run -- gradle build -x lint
```

See also [this GitHub issue](https://github.com/facebook/infer/issues/58).

### Running "infer [options] -- \<build command\>" fails with some other error

Please make sure that:

- \<build command\> runs successfully on its own.
- `infer` is in your `$PATH` (try `which infer`, it should show where `infer` is
  located)

### Running Infer fails with "ImportError: No module named xml.etree.ElementTree"

Make sure that the `xml` Python package is installed. For instance, on OpenSuse
13.1, it is provided by the
[`python-xmldiff`](http://software.opensuse.org/download.html?project=XML&package=python-xmldiff)
package.

### I get errors compiling Infer

Make sure the dependencies are up to date. They may change as we update Infer
itself; you may also need to recompile the facebook-clang-plugins when it
changes version. See the
[installation document](https://github.com/facebook/infer/blob/master/INSTALL.md)
for an up-to-date list of dependencies and how to get them.

### My problem is not listed here

Do not hesitate to [contact us](support#need-help?).

## FAQ

Here are some frequently asked questions. More to come.

### How do I suppress Infer warnings on a class or method?

In Java code, you can do this by annotating your class or method with
`@SuppressLint("infer")`. Or `@SuppressWarnings("infer")` if your Infer is older
than v0.10.0.

### Is Infer supported for Windows?

Infer is not supported on Windows at the moment. You may try installing Infer on
a Linux virtual machine if your project can be compiled on Linux.

### How does Infer compare to the Clang Static Analyzer?

On iOS there is the Clang Static analyzer. Infer does some things different, in
particular reasoning that spans across multiple files. But CSA checks for more
kinds of issues and is also more mature than Infer when it comes to iOS: we send
big respect to CSA! Infer has only got started there recently. Really, these
tools complement one another and it would even make sense to use both. Indeed,
that's what we do inside Facebook.

### How does Infer compare to Android linters and Findbugs?

Infer finds deeper infer-procedural bugs sometimes spanning multiple files.
Linters, in contrast, typically implement simple syntactic checks that are local
within one procedure. But they are valuable and Infer doesn't try to duplicate
what they are good at. At Facebook we run both Infer and a collection of Android
linters. Findbugs can be useful too; it is more akin to linters.

### Why Infer doesn't find a particular bug?

The answer here is for one of the checkers of Infer, the bi-abduction checker,
that finds Null Dereferences, Memory Leaks, Retain Cycles and other memory
related issues.

The analysis performs a symbolic execution of the code, keeping data structures
that represent a symbolic heap, and trying to prove memory safety of the
program. When it fails to prove it, it can report an error, if it finds a Null
Dereference or Memory Leak, or it can find itself in an inconsistent state. In
any case, it will stop the analysis of that particular procedure because the
attempted proof doesn't make sense anymore. Another cause of the analysis not
reaching some part of the code is that we introduce timeouts in the analysis,
because otherwise it would take too long. So it could reach a timeout before
reaching the end of the method. So when Infer doesn't find a particular bug,
it's possible that it is because it couldn't reach that part of the code.
