mpicc -o helloWorld2 helloWorld2.c -fopenmp
echo "Running helloWorld2..."
echo ""
mpirun -np 10 ./helloWorld2