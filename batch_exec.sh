#!/bin/bash
#PBS -l nodes=1,cput=47:00:00
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/mf1009/opt/lhapdf/lib 
export LHAPATH=/home/mf1009/opt/lhapdf/pdfs/

#cd /home/mf1009/hplus/prospino_aachen/prospino_lhapdf

echo "###################Start time:"
date


jdir=XXN
jfrom=XXF
jto=XXT
echo "Doing jobs $jfrom to $jto in $jdir"

echo "Will run in /tmp/${PBS_JOBID}"
mkdir /tmp/${PBS_JOBID}
cd /tmp/${PBS_JOBID}
cp -p /home/mf1009/hplus/prospino_submit14.tgz /tmp/${PBS_JOBID}
tar -xzf prospino_submit14.tgz

#source ~/athena/17.0.4.6/tosrc.sh

#gcc --version

#make clean
#make

x=$jfrom
while [ $x -le $jto ]; do
  jobs="$jobs $x"
  let x=$x+1
done

#for i in `seq 1 50`; do
#for i in `echo 1 2 3 4 5`; do
for i in $jobs; do
  echo "./prospino_2.run < inputs_${jdir}/input${i} >job_${jdir}${i}"
#  ./prospino_2.run < inputs_${jdir}/input${i} >job_${jdir}${i}
  echo "ls to file..."
  ls -l inputs_${jdir}/input${i}
  echo "..done"
  ./prospino_2.run < inputs_${jdir}/input${i} >job_${jdir}${i}
  cp -p job_${jdir}${i} /home/mf1009/hplus/prospino_14tev/
done

echo "###################End time:"
date
