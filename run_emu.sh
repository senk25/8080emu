#!/bin/bash 

SOURCE_DIR="./"
BUILD_DIR="./build"

mkdir -p "$BUILD_DIR"

cmake -S "$SOURCE_DIR" -B "$BUILD_DIR" > /dev/null 2>&1

cmake --build "$BUILD_DIR" > /dev/null 2>&1

"$BUILD_DIR"/8080emu $@


