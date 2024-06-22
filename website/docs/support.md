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
you are able to run Infer, please include the contents of
`infer-out/toplevel.log` in your report. If not, please include at least your
operating system and the version of Infer that you are using.

### Updates

Keep up to date with the latest Infer news on our
[Facebook page](https://www.facebook.com/inferstaticanalyzer/) and our
[Twitter account](https://twitter.com/fbinfer).

### IRC

Our IRC channel is [`#infer`](irc://irc.libera.chat/infer) on [Libera Chat](https://libera.chat/).

## Troubleshooting

### Infer cannot analyze my CocoaPods project

In the presence of CocoaPods, you should use xcworkspace and not xcodeproj in
the compilation command that you supply to Infer. Here is an example you can
adapt to your project:

```bash
infer run -- xcodebuild -workspace HelloWorld.xcworkspace -scheme HelloWorld
```

### `infer [options] -- <build command>` fails during a linking step

The linker will sometimes not work if files have been compiled using a different
compiler, such as the one Infer uses [under the hood](/docs/next/infer-workflow) to
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

### Running `infer [options] -- <build command>` fails with some other error

Please make sure that:

- `<build command>` runs successfully on its own.
- `infer` is in your `$PATH` (try `which infer`, it should show where `infer` is
  located)

### I get errors compiling Infer

Make sure the dependencies are up to date. They may change as we
update Infer itself. See the [installation
document](https://github.com/facebook/infer/blob/main/INSTALL.md)
for an up-to-date list of dependencies and how to get them.

### My problem is not listed here

Do not hesitate to [contact us](#need-help).

## FAQ

Here are some frequently asked questions. More to come.

### How do I suppress Infer warnings on a class or method?

In Java code, you can do this *for some error types* by annotating
your class or method with `@SuppressLint("<ISSUE_TYPE>")`, for example
`@SuppressLint("NULL_DEREFERENCE")`. However, not all checkers honor
this annotation.

### Is Infer supported for Windows?

Infer is not supported on Windows at the moment. You may try installing Infer on
a Linux virtual machine if your project can be compiled on Linux.

### How does Infer compare to the Clang Static Analyzer?

Infer and Clang Static Analyzer (CSA) will typically find different
kinds of issues on the same project. One thing that sets Infer apart
from other static analysis tools is its ability to reason and find
issues across multiple files. But CSA will find many kinds of issues
that Infer doesn't find: we send big respect to CSA! Really, these
tools complement one another and it makes sense to use both.

### How does Infer compare to Android linters and Findbugs?

Infer finds deeper infer-procedural bugs sometimes spanning multiple files.
Linters, in contrast, typically implement simple syntactic checks that are local
within one procedure. But they are valuable and Infer doesn't try to duplicate
what they are good at. At Facebook we run both Infer and a collection of Android
linters. Findbugs can be useful too; it is more akin to linters.
