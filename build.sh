#!/bin/bash

set -x
set -e

if ! test -a .nuget/nuget.exe ; then
    wget -O .nuget/nuget.exe http://nuget.org/nuget.exe
fi

nuget restore Recalled.sln

function build () {
    xbuild /p:Configuration=$1
    rm -rf .recall
    mono Benchmarks/Redefine/bin/$1/Redefine.exe
    mono Benchmarks/Redefine/bin/$1/Redefine.exe
    rm -rf .recall
    mono Benchmarks/BigFib/bin/$1/BigFib.exe
    mono Benchmarks/BigFib/bin/$1/BigFib.exe
    rm -rf .recall
}

build Debug
build Release
