#!/bin/bash

set -e
set -x


npm install -g elm@0.18.0 elm-test@0.18.3 elm-format
elm-format --validate src tests
elm-test
