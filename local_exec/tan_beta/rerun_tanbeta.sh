#!/bin/bash
export LHAPATH=/afs/hephy.at/user/m/mflechl/opt/lhapdf/local/share/lhapdf/

tbval=(
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 25 30 35 40 45 50 55 60
)

echo "Results for mH- = 200 GeV" >res200.txt
for i in "${tbval[@]}"; do
  echo "tan beta: $i"
  cat input_mYY_tbXX | sed s#XX#${i}# | sed s#YY#200# >input_tmp
  ./prospino_2.run < input_tmp &> fm200_tb${i}
  tail -4 fm200_tb${i} | head -1 >>res200.txt
done

echo "Results for mH- = 600 GeV" >res600.txt
for i in "${tbval[@]}"; do
  echo "tan beta: $i"
  cat input_mYY_tbXX | sed s#XX#${i}# | sed s#YY#600# >input_tmp
  ./prospino_2.run < input_tmp &> fm600_tb${i}
  tail -4 fm600_tb${i} | head -1 >>res600.txt
done
