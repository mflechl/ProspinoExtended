#!/bin/bash
export LHAPATH=/afs/hephy.at/user/m/mflechl/opt/lhapdf/local/share/lhapdf/

echo "###################Start time:"
date


jdir=XXN
jfrom=XXF
jto=XXT

rundir=${jdir}_${jfrom}_${jto}
thisdir=$PWD

echo "Doing jobs $jfrom to $jto in $jdir, rundir is $rundir, thisdir is $thisdir"

mkdir /tmp/${rundir}
cp -p ../prospino.tgz /tmp/${rundir}
cd /tmp/${rundir}
tar -xzf prospino.tgz

#echo "Will run in /tmp/${PBS_JOBID}"
#mkdir /tmp/${PBS_JOBID}
#cd /tmp/${PBS_JOBID}
#cp -p /home/mf1009/hplus/prospino_submit14.tgz /tmp/${PBS_JOBID}
#tar -xzf prospino_submit14.tgz

#gcc --version

make clean
make

x=$jfrom
while [ $x -le $jto ]; do
  jobs="$jobs $x"
  let x=$x+1
done

nice nohup ~/scripts/kren.sh &> logk.txt &

#for i in `seq 1 50`; do
#for i in `echo 1 2 3 4 5`; do
mkdir -p jobs
for i in $jobs; do
  echo "./prospino_2.run < inputs_${jdir}/input${i} >job_${jdir}${i}"
#  ./prospino_2.run < inputs_${jdir}/input${i} >job_${jdir}${i}
  echo "ls to file..."
  ls -l inputs_${jdir}/input${i}
  echo "..done"
  ./prospino_2.run < inputs_${jdir}/input${i} >job_${jdir}${i}
  mv job_${jdir}${i} ${thisdir}/jobs_output/
done

echo "###################End time:"
date
