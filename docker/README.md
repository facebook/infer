# docker images for Infer

This directory, `docker/` inside the Infer repo,
contains a docker file to install Infer within a
[docker](https://www.docker.com/) container. This can be used to
quickly try Infer or to deploy Infer.


## Pre-requisites

To use this docker image, you will need a working docker
installation. See the instructions for
[Linux](http://docs.docker.com/linux/step_one/) or
[MacOSX](http://docs.docker.com/mac/step_one/) as appropriate.


## How to use

This docker file will use the latest
[released](https://github.com/facebook/infer/releases) version of
Infer. 

1. Get docker running, e.g. using Docker Quickstart Terminal.
2. go to the version of your choice, e.g. `cd docker/1.1.0/`
3. Build or install Infer in the Docker container and try on an example:

```sh
cd docker/1.1.0/
docker build -t infer .
# mount the local examples directory inside the image
# you can mount your project directory here instead
docker run -it -v $PWD/../../examples:/infer-examples infer /bin/bash
# you should now be inside the docker container with a shell prompt, e.g.
# "root@5c3b9af90d59:/# "
cd /infer-examples/
infer -- clang -c hello.c
```
