
#include <iostream>
#include <pthread.h>


using namespace std;

/* declare a mutex */ mymutex;

/// Para pensar: Na implementação computacional 1 (IC 1) o que acontecia com a variável global 
/// count quando ela era incrementada pelos diferentes processos?
/// 
/// 
/// O mesmo vai acontecer agora quando a variável global x for incrementada e decrementada pelos
/// diferentes threads?
/// 
/// 
/// Qual é o valor esperado para a variável x após o término do aplicativo?
/// 
/// 
/// Se não houver mutex, qual será o valor final da variável global x (ou sua distribuição de
/// probabilidade)?
/// 
/// 
/// 
void *inc_(void *void_ptr)
{
	/* increment x to 100 */
	int *ptr = (int *)void_ptr;
	int i=0;
	for (; i<100; i++) 
	{
		/* enter critical region */
		++(*ptr);
		/* leave critical region */
	}
	cout << "increment finished" << endl;
	return NULL;
}

void *dec_(void *void_ptr)
{
    /* decrement x to 100 */
    int *ptr = (int *)void_ptr;
	int i=0;
	for (; i<100; i++)
	{
		/* enter critical region */
		--(*ptr);
		/* leave critical region */
	}        
    cout << "decrement finished" << endl;
    return NULL;
}


int main()
{
	int x = 0;
	cout << "x: " << x << endl;

	/* declare threads */ inc_thread, dec_thread;
	/* init mutexex */

    /* create a first thread which executes inc_(&x) */

	/* create a second thread which executes dec_(&x) */

    /* wait for the first thread to finish */

	/* wait for the second thread to finish */

	/* destroy miutex */

	cout << "x: " << x << endl;
	return 0;
}
