#!/bin/bash

for ((i=0;i<=12;i++))
do
    ./cram 2 $i
done

for ((i=0;i<=9;i++))
do
    ./cram 3 $i
done

./cram 4 4
./cram 4 5
./cram 4 6

./cram 5 5

