#!/usr/bin/env bash

echo "WEASEL Test Harness"
echo "Running command: $@"

T=$(time (for i in {1..10}
do
	$@ >/dev/null
done) 2>&1)

echo "10 iterations took: "
E=$(echo "$T" | tail -n 3 | head -n 1 | cut -c8- | cut -c-5)
echo "$E / 10" | bc -l | cut -c-5
echo "Finished."
