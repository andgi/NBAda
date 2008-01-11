#!/bin/bash
# $Id: extract_result.sh,v 1.1 2008/01/11 18:48:56 andersg Exp $
# Extract the result in gnuplot form from an experiment log file.
# Anders Gidenstam  2008
#
# Usage: extract_result.sh <experiment log>
#

exp=$1

gawk \
'
BEGIN {
  last = 0;
  sumSamples = 0;
  numEnq = 0;
  numDeq = 0;
  sumTime = 0;
  faulty = 0;
}
/^ [[:digit:]]+   [[:digit:]]+   [[:digit:]]+/ {
  if ($1 != last) {
    if (last > 0) {
      print last, " ", last, " ", numEnq, " ", numDeq, " ", sumTime/sumSamples, " ", sumSamples, " ", faulty;
    }
    last = $1;
    numEnq = $3;
    numDeq = $4;
    sumSamples = 0;
    sumTime = 0;
    faulty = 0;
  }
  sumSamples += 1;
  sumTime += $5;
  if ((numDeq != $4) || (numEnq != $3)) { faulty=1; }
  #print $0;
}
END {
  print last, " ", last, " ", numEnq, " ", numDeq, " ", sumTime/sumSamples, " ", sumSamples, " ", faulty;
}' \
$exp
