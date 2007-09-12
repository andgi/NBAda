#!/bin/bash
# $Id: benchmark.sh,v 1.3 2007/09/12 18:32:03 andersg Exp $
# Benchmark batch job
# Copyright (C) 2007  Anders Gidenstam  
#

# job directory
DIR=./
BIN=./

# The job
cd $DIR

# Lists of programs and worker threads
PROGS="queue_test.ebmr queue_test.hpmr queue_test.lfrc queue_test.lfmr queue_test.bounded"
THREADS="1 2 4 8 16"

# The number of repetitions of each experiment.
SAMPLES="1 2 3 4 5 6 7 8 9 10"

LOGBASE=result_`uname -n`_`uname`_`date +%Y-%m-%d_%H:%M`.log

for T in $THREADS; do
  for S in $SAMPLES; do
    for PRG in $PROGS; do
      LOG=${PRG}_${LOGBASE}
      echo $PRG $T threads sample $S
      echo "% Sample: "$S >> $LOG
      $BIN/$PRG -p $T -c $T -s >> $LOG
    done
  done
done