#!/bin/sh
ADDR="http://localhost:8080/clamorous/publish"
if [ -n "$1" ]; then
	ADDR=$1
fi

while read LN; do
	echo $LN | curl -XPOST $ADDR -d @-
	echo ""
done
