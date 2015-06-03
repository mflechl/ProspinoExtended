#!/usr/bin/env zsh
qsub -W 0:20 -M 2048 -o log1.txt < ./testjob.sh
#MF bsub -W 0:20 -M 2048 -o job_ct10_alphas%I < ct10_alphas_jobs1.sh
#MF bsub -W 0:20 -M 2048 -o job_ct10_alphas%I < ct10_alphas_jobs2.sh
