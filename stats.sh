#!/bin/sh

# Generates statistics on pandoc: benchmarks and lines of code
# The stats are put in the stats directory, marked with date and revision hash.

STATSDIR=stats
mkdir $STATSDIR

DATE=`date +%Y_%m_%d`
REV=`git rev-parse --short HEAD`

EXT=$DATE.$REV
BENCH=$STATSDIR/benchmark.$EXT
LOC=$STATSDIR/loc.$EXT
SUMMARY=$STATSDIR/summary.$EXT

runghc Benchmark.hs > $BENCH
find src -name '*.hs' | xargs wc -l > $LOC

LOCSUM=`tail -1 $LOC | sed -Ee 's/^ *([0-9]+).*/\1/'`

echo "Revision $REV" > $SUMMARY
echo `date` >> $SUMMARY
echo "$LOCSUM lines of code" >> $SUMMARY
echo "" >> $SUMMARY

sed -nEe '/^(benchmarking|mean:)/p' $BENCH | \
  sed -Ee '/benchmarking/N;s/\n/: /' | \
  sed -Ee 's/benchmarking (.*)/\1/' | \
  sed -Ee 's/mean: ([^ ]*) *([^,]*).*/\1:\2/' | \
  awk 'BEGIN { FS = ": *" } ; { printf("%s:%7.2f %s\n", $1, $2, $3); }' | \
  column -t -s ":" >> $SUMMARY

