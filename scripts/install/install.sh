#!/bin/bash

if [ "$#" -ne 1 ]; then
	HP="/usr/local/herbrand"
else
	HP=$1
fi
cd ../../src
gcc -I/usr/include -L *.h *.c -o herbrand -DHERBRAND_PATH=\"$HP\"
cd ../scripts/install
if [ -d $HP ]; then
	rm -r $HP
fi
mkdir -p $HP
cp ../../LICENSE $HP/LICENSE
cp -r ../../modules $HP/modules
cp ../../src/herbrand /usr/local/bin/herbrand
