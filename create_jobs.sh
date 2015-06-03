#!/bin/bash

jobtype=(
"pdf_error"
"ct10_alphas"
"mstw_alphas"
"nnpdf_alphas"
"mstw_mb"
"nnpdf_mb"
###"propaganda"
###"prospino"
###"scale_var"
"scale_var_new"
###"scaleplot"
##"scale_var_vs_tb"
"tanbeta"
)

for i in ${jobtype[@]}; do
  echo $i
  mkdir inputs_${i}
  python2.6 ./create_${i}_inputs.py
  rm -f ${i}*jobs*
done
