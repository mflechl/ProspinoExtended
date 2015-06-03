#!/usr/bin/env zsh
bsub -W 0:20 -M 2048 -o job_tanbeta%I < tanbeta_jobs1.sh
bsub -W 0:20 -M 2048 -o job_tanbeta%I < tanbeta_jobs2.sh
bsub -W 0:20 -M 2048 -o job_tanbeta%I < tanbeta_jobs3.sh
