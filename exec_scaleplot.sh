#!/usr/bin/env zsh
bsub -W 0:20 -M 2048 -o job_scaleplot%I < scaleplot_jobs1.sh
bsub -W 0:20 -M 2048 -o job_scaleplot%I < scaleplot_jobs2.sh
