#!/bin/bash

rm plasma_lbm.x
rm step*.nc
cp ../../../build/bin/plasma_lbm.x .
cafrun -np 1 ./plasma_lbm.x basic_input.ini
