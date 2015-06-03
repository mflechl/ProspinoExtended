#!/bin/bash

local=1

split=400

#find number of jobs
njobs=`ls inputs_${1} | wc -l | sed s'# ##g'`
#njobs=`ls inputs_ct10_alphas | wc -l | sed s'# ##g'`
echo "Number of jobs for $1: $njobs"

#if [ $local -eq 1 ]; then split=$njobs; fi
#if [ $local -eq 1 ]; then split=300; fi

from=1
subno=1

#recover failed jobs
#split=8
#from=1170
#subno=7777
#njobs=1177

while [ "$from" -le "$njobs" ]; do
  let to=$from+$split-1
  if [ $to -gt $njobs ]; then to=$njobs; fi
  if [ $local -ne 1 ]; then
      echo "qsub -o log_${1}_${subno}.txt ./batch_exec.sh $1 $from $to"
      cat batch_exec.sh | sed s#XXN#${1}#g | sed s#XXF#${from}#g | sed s#XXT#${to}#g >batch_exec.sh_${1}_${from}_${to}
      qsub -o log_${1}_${subno}.txt ./batch_exec.sh_${1}_${from}_${to}
  else
      echo "Ready for submission: ./local_exec.sh_${1}_${from}_${to} "
      cat local_exec.sh | sed s#XXN#${1}#g | sed s#XXF#${from}#g | sed s#XXT#${to}#g >local_exec.sh_${1}_${from}_${to}
      chmod a+x local_exec.sh_${1}_${from}_${to}
  fi
  let from=$from+$split
  let subno=$subno+1
done
