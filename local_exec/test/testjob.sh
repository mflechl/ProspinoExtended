#!/bin/bash

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/mf1009/opt/lhapdf/lib 
export LHAPATH=/home/mf1009/opt/lhapdf/pdfs/

cd /home/mf1009/hplus/prospino_aachen/prospino_lhapdf

jfrom=$1
jto=$2
echo "Doing jobs $jfrom to $jto"

x=$jfrom
while [ $x -le $jto ]; do
  jobs="$jobs $x"
  let x=$x+1
done

#for i in `seq 1 50`; do
#for i in `echo 1 2 3 4 5`; do
for i in $jobs; do
  echo "./prospino_2.run < inputs_ct10_alphas/input${i} >job_ct10_alphas${i}"
  # ./prospino_2.run < inputs_ct10_alphas/input${i} >job_ct10_alphas${i}
done

#./prospino_2.run < inputs_ct10_alphas/input2 >job_ct10_alphas2
