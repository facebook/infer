---
id: getting-started
title: Getting started with Infer
---

## Get Infer

You can use our binary releases, build infer from source, or use our Docker image.

Find our latest [binary release here](https://github.com/facebook/infer/releases/latest). Download
the tarball then extract it anywhere on your system to start using infer. For example, this
downloads infer in /opt on Linux (replace `VERSION` with the latest release, eg `VERSION=1.0.0`):

```bash
VERSION=0.XX.Y; \
curl -sSL "https://github.com/facebook/infer/releases/download/v$VERSION/infer-linux64-v$VERSION.tar.xz" \
| sudo tar -C /opt -xJ && \
sudo ln -s "/opt/infer-linux64-v$VERSION/bin/infer" /usr/local/bin/infer
```

If the binaries do not work for you, or if you would rather build infer from
source, follow the
[install from source](https://github.com/facebook/infer/blob/main/INSTALL.md#install-infer-from-source)
instructions to install Infer on your system.

Alternatively, use our [Docker images](https://github.com/facebook/infer/tree/main/docker).

## Try Infer in your browser

Try Infer on a small example on
[Codeboard](https://codeboard.io/projects/11587?view=2.1-21.0-22.0).
