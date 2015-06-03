#!/usr/bin/env zsh
bsub -W 0:20 -M 2048 -o job_scale_var%I < scale_var_jobs1.sh
