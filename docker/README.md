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
2. cd to the directory `docker/`,
3. Build Infer in docker container and try on an example:

```sh
# Build Infer; 20min or so; to be executed from docker/ in the Infer repo
./run.sh
# you should now be inside the docker container with a shell prompt, e.g.
# "root@5c3b9af90d59:/infer/examples# "
sdkmanager --licenses
cd android_hello/
infer -- ./gradlew build
```
