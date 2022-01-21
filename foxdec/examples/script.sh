#!/bin/bash

for d in $(find . -mindepth 1 -maxdepth 1 -type d)
do
  # for all subdirs
  for f in $(find $d -name "*.entry" -type f)
  do
    # for the .entry file in the subdir

    # wait while max num of subprocesses is reached
    while [ `jobs | wc -l` -ge 4 ]
    do
      sleep 10
    done

    # extract basename
    file=${f##*/} 
    base=${file%.*}

    echo "Running example $d/$base"
    { time foxdec-exe 0 $d $base ; echo "$d/$base done." > /dev/tty ; } &> $d/$base.out &
  done
done


for job in `jobs -p`
do
  wait $job 
done

