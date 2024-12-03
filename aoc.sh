#!/bin/zsh

year=$(date +%Y)
month=$(date +%m)
day=$(date +%d)

if [[ $month -ne 12 ]]; then
  echo "It's not December you silly!"
  exit
fi

if [ -d "Day${day}" ]; then
  echo "Day${day} already exists."
  exit
fi

dune init proj "Day${day}"
cd "Day${day}"
aocdl -year $year -day $day

rm ./dune-project
sed "s/%DAY%/$day/g" ../dune-template > ./dune-project

cp ../template.ml bin/main.ml

dune exec Day$day

echo "Finished"
