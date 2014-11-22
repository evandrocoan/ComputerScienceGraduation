#!/bin/bash

gcc -o paralelo amigos-paralelo.c -fopenmp -std=c99 -lm
gcc -o mutuamenteAmigosParalelo mutuamenteAmigosParalelo.c -fopenmp -std=c99 -lm
gcc -o concParal concParal.c -fopenmp -std=c99 -lm

echo "Meninas"
time ./concParal 1 100000 4
echo "Evandro"
time ./paralelo 1 100000 4
echo "Paladini"
time ./mutuamenteAmigosParalelo 1 100000 4