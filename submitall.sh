#!/bin/bash

jobtype=(
"ct10_alphas"
"mstw_alphas"
"mstw_mb"
"nnpdf_alphas"
"nnpdf_mb"
"pdf_error"
##"scale_var"
"scale_var_new"
##"scaleplot"
"tanbeta"
)

for i in ${jobtype[@]}; do
  ./submit.sh $i
done
