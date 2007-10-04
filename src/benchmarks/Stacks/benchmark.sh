#!/bin/bash
# $Id: benchmark.sh,v 1.1 2007/10/04 11:54:35 andersg Exp $
# Benchmark batch job
# Copyright (C) 2007  Anders Gidenstam  
#

# job directory
DIR=./
BIN=./

# The job
cd $DIR

# Lists of programs and worker threads
PROGS="stack_test.ebmr stack_test.hpmr stack_test.mutex stack_test.splk"
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
      $BIN/$PRG -u $T -o $T -s >> $LOG
    done
  done
done