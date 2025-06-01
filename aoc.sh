#!/bin/zsh

year=$(date +%Y)
month=12
day=$(date +%d)

# Parse CLI arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -year)
      if [[ -n $2 ]]; then
        year=$2
        shift 2
      else
        echo "Error: Missing value for -year."
        exit 1
      fi
      ;;
    -day)
      if [[ -n $2 ]]; then
        day=$2
        shift 2
      else
        echo "Error: Missing value for -day."
        exit 1
      fi
      ;;
    *)
      echo "Unknown argument: $1"
      echo "Usage: $0 [-year YEAR -day DAY] [-day DAY]"
      exit 1
      ;;
  esac
done

# Validate argument combinations
if [[ -n $year && -z $day ]]; then
  echo "Error: If -year is provided, -day must also be specified."
  exit 1
fi


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
sed "s/%DAY%/$day/g" ../dune-project-template > ./dune-project

rm ./bin/dune
sed "s/%DAY%/$day/g" ../dune-template > ./bin/dune

rm ./bin/main.ml
sed "s/%DAY%/$day/g" ../template.ml > ./bin/main.ml

dune exec Day$day

echo "Finished"
