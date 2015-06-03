#!/usr/bin/env zsh
bsub -W 0:20 -M 2048 -o job_mstw_alphas%I < mstw_alphas_jobs1.sh
bsub -W 0:20 -M 2048 -o job_mstw_alphas%I < mstw_alphas_jobs2.sh
bsub -W 0:20 -M 2048 -o job_mstw_alphas%I < mstw_alphas_jobs3.sh
