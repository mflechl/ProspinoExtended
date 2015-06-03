#!/bin/bash

jobtype=(
"pdf_error"
"ct10_alphas"
"mstw_alphas"
"nnpdf_alphas"
"mstw_mb"
"nnpdf_mb"
#"propaganda"
#"prospino"
"scale_var"
)

subdir="/home/mf1009/hplus/prospino_submit"

for i in ${jobtype[@]}; do
  echo "$i : done: "
  ls job_${i}* | wc -l
  echo "expected: "
  ls ${subdir}/inputs_${i}/* | wc -l
done



