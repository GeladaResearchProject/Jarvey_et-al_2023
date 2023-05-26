#!/bin/bash

slots=$(echo $SLURM_JOB_CPUS_PER_NODE | sed 's/[()]//g' | sed 's/x/*/g' | sed 's/,/+/g' | bc)

step_size=$1

start=$(( ($SLURM_ARRAY_TASK_ID - 1) * $step_size + 1 ))
end=$(($start + ($step_size - 1)))

parallel -j $slots ./season_permutation_output.R {1} ::: $(eval echo -e {${start}..${end}})
