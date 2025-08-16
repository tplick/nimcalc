#!/bin/bash
for ((i=1; i<=$1; i++)); do ./chomp $1 $i $i | grep ": 0"; done

