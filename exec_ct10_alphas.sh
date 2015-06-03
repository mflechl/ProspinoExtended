#!/usr/bin/env zsh
bsub -W 0:20 -M 2048 -o job_ct10_alphas%I < ct10_alphas_jobs1.sh
