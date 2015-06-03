#!/usr/bin/env zsh
bsub -W 0:20 -M 2048 -o job_mstw_mb%I < mstw_mb_jobs1.sh
