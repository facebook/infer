FROM ubuntu
ARG UID=1000
RUN apt-get update && apt-get install --yes autoconf automake unzip aspcud rsync \
    git mercurial darcs wget build-essential sudo vim curl
RUN useradd -m -s /bin/bash ci --uid=${UID}
RUN echo ci ALL=\(ALL\) NOPASSWD:ALL >/etc/sudoers
USER ci
RUN wget -O ~/opam https://github.com/ocaml/opam/releases/download/2.1.2/opam-2.1.2-x86_64-linux
RUN chmod +x ~/opam
RUN sudo mv ~/opam /usr/local/bin/opam
RUN opam init --disable-sandboxing --auto-setup --dot-profile=/home/ci/.bash_env
SHELL ["/bin/bash", "-c"]
ENV BASH_ENV /home/ci/.bash_env
RUN opam update && opam switch create 3.07
RUN opam update && opam switch create 3.08.4
RUN opam update && opam switch create 3.09.3
RUN opam update && opam switch create 3.10.2
RUN opam update && opam switch create 3.11.2
RUN opam update && opam switch create 3.12.1
RUN opam update && opam switch create 4.00.1
RUN opam update && opam switch create 4.01.0
USER root