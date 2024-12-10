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

sed "s/%DAY%/$day/g" day-template.ml > ../days/day_$day.ml

aocdl -year $year -day $day -output inputs/input_{{.Day}}.txt

rm ./dune-project
sed "s/%DAY%/$day/g" ../dune-template > ./dune-project

cp ../template.ml bin/main.ml

dune exec Day$day

echo "Finished"
