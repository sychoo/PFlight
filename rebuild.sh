#!/bin/bash

# delete old FlightSystem build
rm -rf build/FlightSystem

# Generate code from P
cd C
pc -proj:FlightSystem.pproj
cd ..

# Rebuild flight system
mkdir -p build/FlightSystem
cd build/FlightSystem
cmake -DCMAKE_PREFIX_PATH=../mavlink/install/lib/cmake/MAVLink/ ../../C
make -j$(nproc --all)
