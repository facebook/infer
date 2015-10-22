# Base image
FROM heikomaass/android-sdk

MAINTAINER Infer

# Add build-tools-22 to the Android SDK
RUN ["/opt/sdk-tools/android-accept-licenses.sh", "android update sdk --filter \"build-tools-22.0.1\" --no-ui --force -a"]

# Debian config
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
            build-essential \
            curl \
            git \
            groff \
            libgmp-dev \
            libmpc-dev \
            libmpfr-dev \
            m4 \
            ocaml \
            python-software-properties \
            rsync \
            software-properties-common \
            unzip \
            zlib1g-dev

# Install a recent Gradle
RUN add-apt-repository ppa:cwchien/gradle
RUN apt-get update && \
    apt-get install gradle-2.5

# Install OPAM
RUN curl -sL \
      https://github.com/ocaml/opam/releases/download/1.2.2/opam-1.2.2-x86_64-Linux \
      -o /usr/local/bin/opam && \
    chmod 755 /usr/local/bin/opam
RUN opam init -y --comp=4.01.0 && \
    opam install -y extlib.1.5.4 atdgen.1.6.0 javalib.2.3.1 sawja.1.5.1

# Download the latest Infer release
RUN INFER_VERSION=$(curl -s https://api.github.com/repos/facebook/infer/releases \
      | grep -e '^[ ]\+"tag_name"' \
      | head -1 \
      | cut -d '"' -f 4); \
    cd /opt && \
    curl -sL \
      https://github.com/facebook/infer/releases/download/${INFER_VERSION}/infer-linux64-${INFER_VERSION}.tar.xz | \
    tar xJ && \
    rm -f /infer && \
    ln -s ${PWD}/infer-linux64-$INFER_VERSION /infer

# Compile Infer
RUN cd /infer && \
    eval $(opam config env) && \
    make -C infer clang java

# Install Infer
ENV INFER_HOME /infer/infer
ENV PATH ${INFER_HOME}/bin:${PATH}
