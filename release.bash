#!/bin/bash

set -e
set -x

docker build --tag carna:latest .
docker tag carna:latest docker.io/scepticulous/carna:latest
docker push docker.io/scepticulous/carna:latest

docker tag carna:latest docker.pkg.github.com/dennissivia/carna/carna:latest
docker push docker.pkg.github.com/dennissivia/carna/carna:latest
