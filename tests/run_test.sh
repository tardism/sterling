#!/bin/bash

if ! silver --mwda -o testing.jar -I ../grammars sos:testing; then
    echo "Unable to build testing jar"
    exit 4
fi

#assume we are running from the tests directory
export STERLING_HOME=..
export STERLING_GRAMMARS=${STERLING_HOME}/grammars/
export STERLING_GENERATED=${STERLING_HOME}/generated/

java -jar testing.jar basics:base
java -jar testing.jar basics:ext1

