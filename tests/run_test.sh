#!/bin/bash

if ! silver --clean -o testing.jar -I ../grammars sos:testing; then
    echo "Unable to build testing jar"
    exit 4
fi

java -jar testing.jar basics:base
java -jar testing.jar basics:ext1

