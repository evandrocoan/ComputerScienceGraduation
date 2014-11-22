#!/bin/bash

gcc -o paralelo amigos-paralelo.c -fopenmp -std=c99 -lm

echo "paralelo"
time ./paralelo 1 100000 4