---
layout: support
title: Infer | Need help?
id: support
category: support
---

## Need help?

Do not hesitate to ask questions using the following channels, or to
submit pull request!

### GitHub issues

The [GitHub issues](https://github.com/facebook/Infer/issues) page is
a good place to ask questions, find answers, and report issues.

### Twitter

Keep up to date with the latest Infer news on
[@fbinfer](https://twitter.com/fbinfer).

### IRC

Our IRC channel is [#infer](irc://chat.freenode.net/infer) on
Freenode.net.


## Troubleshooting

### Running "infer -- \<build command\>" fails.

Please make sure that:

- \<build command\> runs successfully on its own.
- `infer` is in your `$PATH` (try `which infer`, it should show where `infer` is located)

### Running Infer fails with "ImportError: No module named xml.etree.ElementTree"

Make sure that the `xml` Python package is installed. For instance, on
OpenSuse 13.1, it is provided by the
[`python-xmldiff`](http://software.opensuse.org/download.html?project=XML&package=python-xmldiff)
package.

### My problem is not listed here

Do not hesitate to [contact us](support.html#need-help?).


## FAQ

Here are some frequently asked questions. More to come.

### Is Infer supported for Windows?

Infer is not supported on Windows at the moment. You may try
installing Infer on a Linux virtual machine.

### How does Infer compare to the Clang Static Analyzer?

On iOS there is the Clang Static analyzer. Infer does some things different, 
in particular reasoning that spans across multiple files. But CSA checks for 
more kinds of issues and is also more mature than Infer when it comes to iOS: 
we send big respect to CSA! Infer has only got started there recently. Really, 
these tools complement one another and it would even make sense to use both. 
Indeed, that's what we do inside FB.

### How does Infer compare to Android linters and Findbugs?

Infer finds deeper infer-procedural bugs sometimes spanning multiple files. 
Linters, in contrast, typically implement simple syntactic checks that are 
local within one procedure. But they are valuable and Infer doesn't try to 
duplicate what they are good at. At Facebook we run both Infer and a 
collection of Android linters. Findbugs can be useful too; it is more akin 
to linters.