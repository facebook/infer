# docker images for Infer

This directory contains a docker file to install Infer within a
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
Infer. Simply type "./run.sh" to get a shell inside the docker
container. It will have built and installed Infer. You can then try
out Infer on the introductory examples:

```sh
# compiles Infer; this takes a few minutes
./run.sh
# you should now be inside the docker container with a shell prompt, e.g.
# "root@5c3b9af90d59:/# "
cd /infer/examples/android_hello/
infer -- gradle build
```
