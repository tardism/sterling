#!/bin/bash

if ! silver --mwda -o testing.jar -I ../grammars sos:testing; then
    echo "Unable to build testing jar"
    exit 4
fi

#assume we are running from the tests directory
export SOS_HOME=..
export SOS_GRAMMARS=${SOS_HOME}/grammars/
export SOS_GENERATED=${SOS_HOME}/generated/

java -jar testing.jar basics:base
java -jar testing.jar basics:ext1

