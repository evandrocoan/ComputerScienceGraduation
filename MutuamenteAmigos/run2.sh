#!/bin/bash

LIB=-lm
NUM_RUNS=3
TRHEADS=4
MIN=1
MAX=100000

gcc -o paralelo_linux amigos-paralelo.c -fopenmp -std=c99 -lm
gcc -o mutuamenteAmigosParalelo_linux mutuamenteAmigosParalelo.c -fopenmp -std=c99 -lm
gcc -o concParal_linux concParal.c -fopenmp -std=c99 -lm

echo "Meninas"
time ./concParal_linux 1 100000 4
echo "Evandro"
time ./paralelo_linux 1 100000 4
echo "Paladini"
time ./mutuamenteAmigosParalelo_linux 1 100000 4