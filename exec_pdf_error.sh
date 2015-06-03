#!/usr/bin/env zsh
bsub -W 0:20 -M 2048 -o job_pdf_error%I < pdf_error_jobs1.sh
bsub -W 0:20 -M 2048 -o job_pdf_error%I < pdf_error_jobs2.sh
bsub -W 0:20 -M 2048 -o job_pdf_error%I < pdf_error_jobs3.sh
bsub -W 0:20 -M 2048 -o job_pdf_error%I < pdf_error_jobs4.sh
bsub -W 0:20 -M 2048 -o job_pdf_error%I < pdf_error_jobs5.sh
bsub -W 0:20 -M 2048 -o job_pdf_error%I < pdf_error_jobs6.sh
