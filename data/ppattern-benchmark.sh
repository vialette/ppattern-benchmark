#!/bin/bash

# number of samples
N=5000

# q permutation
Q=100
QSPLITMAX=8
QSPLITSTEP=2

# p permutation
PMIN=8
PMAX=18
PSTEP=2
PSPLITMIN=2
PSPLITMAX=8
PSPLITSTEP=2

# generate
for ((P = $PMIN; P <= $PMAX ; P+=$PSTEP)); do
  for ((PSPLIT = $PSPLITMIN; PSPLIT <= $PSPLITMAX ; PSPLIT+=$PSPLITSTEP)); do
    for ((QSPLIT = $PSPLIT; QSPLIT <= $QSPLITMAX ; QSPLIT+=$QSPLITSTEP)); do

      # output csv file
      CSV=varying/ppattern-benchmark-${P}-${Q}-${PSPLIT}-${QSPLIT}.csv

      for ((I = 1; I <= $N; I++)); do
        DATE=`date +"%T"`
        echo "#${I} - ${DATE}";

        # random generator seed
        SEED=$RANDOM

        # benchmark
        echo ppattern-benchark --psize=$P --qsize=$Q --psplit=$PSPLIT --qsplit=$QSPLIT --seed=$SEED
        ../dist/build/ppattern-benchark/ppattern-benchark --psize=$P --qsize=$Q --psplit=$PSPLIT --qsplit=$QSPLIT --seed=$SEED >> ${CSV}
        echo
      done
    done
  done
done
