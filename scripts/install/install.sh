#!/bin/bash

cd ../../src
gcc -I/usr/include -L *.h *.c -o herbrand
cd ..
if [ -d "/usr/local/herbrand" ]; then
	rm -r /usr/local/herbrand
fi
mkdir -p /usr/local/herbrand
cp LICENSE /usr/local/herbrand/LICENSE
cp -r modules /usr/local/herbrand/modules
cp src/herbrand /usr/local/bin/herbrand
