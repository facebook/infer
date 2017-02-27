# Base image
FROM heikomaass/android-sdk

MAINTAINER Infer

# Add android-22 and build-tools-22 to the Android SDK
RUN ["/opt/sdk-tools/android-accept-licenses.sh", \
     "android update sdk --filter \"android-22\" --no-ui --force --all"]
RUN ["/opt/sdk-tools/android-accept-licenses.sh", \
     "android update sdk --filter \"build-tools-22.0.1\" --no-ui --force --all"]

# Debian config
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
            autoconf \
            curl \
            gcc \
            g++ \
            git \
            groff \
            libc6-dev \
            libffi-dev \
            libgmp-dev \
            libmpc-dev \
            libmpfr-dev \
            m4 \
            make \
            ncurses-dev \
            ocaml \
            pkg-config \
            python-software-properties \
            rsync \
            software-properties-common \
            unzip \
            zlib1g-dev

# Install OPAM
RUN VERSION=1.2.2; \
    curl -sL \
      https://github.com/ocaml/opam/releases/download/$VERSION/opam-$VERSION-x86_64-Linux \
      -o /usr/local/bin/opam && \
    chmod 755 /usr/local/bin/opam && \
    ((/usr/local/bin/opam --version | grep -q $VERSION) || \
     (echo "failed to download opam from GitHub."; exit 1))
RUN opam init -y --comp=4.02.3

# Download the latest Infer release
RUN INFER_VERSION=v0.10.0; \
    cd /opt && \
    curl -sL \
      https://github.com/facebook/infer/releases/download/${INFER_VERSION}/infer-linux64-${INFER_VERSION}.tar.xz | \
    tar xJ && \
    rm -f /infer && \
    ln -s ${PWD}/infer-linux64-$INFER_VERSION /infer

# Compile Infer
RUN cd /infer && \
    ./build-infer.sh

# Install Infer
ENV INFER_HOME /infer/infer
ENV PATH ${INFER_HOME}/bin:${PATH}
