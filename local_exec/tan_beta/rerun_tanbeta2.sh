#!/bin/bash
export LHAPATH=/afs/hephy.at/user/m/mflechl/opt/lhapdf/local/share/lhapdf/

tbval=(
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 25 30 35 40 45 50 55 60
)

pdfset=(
CT10.LHgrid MSTW2008nlo_asmzrange.LHgrid NNPDF23_nlo_as_0118.LHgrid
)

mass=(
200 600
)

scale=(
4.975660 5.844700
)

function calc {
  m_mass=$1
  m_pdf=$2
  m_scale=$3
  m_tb=$4
  fname=fm_m${m_mass}_tb${m_tb}_pdf${m_pdf}

  cat input_mYY_tbXX_pdfZZ_scUU | sed s#XX#${m_tb}# | sed s#YY#${m_mass}# | sed s#ZZ#${m_pdf}# | sed s#UU#${m_scale}#  >input_tmp
  ./prospino_2.run < input_tmp &> $fname
  tail -4 $fname | head -1 >>res${m_mass}_${m_pdf}.txt
}

function scan {

  echo "Results for mH- = ${1} and pdf set ${2}:" >res${1}_${2}.txt
  for i in "${tbval[@]}"; do
    calc $1 $2 $3 $i
  done
}

#scan ${mass[0]} ${pdfset[0]} ${scale[0]}
# ${mass[0]} ${pdfset[1]} ${scale[0]}
scan ${mass[0]} ${pdfset[2]} ${scale[0]}

scan ${mass[1]} ${pdfset[0]} ${scale[1]}
scan ${mass[1]} ${pdfset[1]} ${scale[1]}
scan ${mass[1]} ${pdfset[2]} ${scale[1]}

#scan ${mass[0]} ${pdfset[0]} ${scale[0]}

