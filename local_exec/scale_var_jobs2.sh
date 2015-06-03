#!/usr/bin/env zsh

#BSUB -J "myArray[101-200]" ARRAYJOB

module switch intel gcc/4.6
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/rk066459/LHAPDF/lib

./prospino_2.run < inputs_scale_var/input$LSB_JOBINDEX
