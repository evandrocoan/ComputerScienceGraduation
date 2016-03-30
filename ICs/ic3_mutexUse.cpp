
#include <iostream>
#include <pthread.h>


using namespace std;

/* Declare a mutex pointer */
pthread_mutex_t *xGlobalVariableMutexLocker;

/** Increment the xGlobalVariable to 100.
 * 
 * @param xGlobalVariableVoidPointer      the variable to increment until 100.
 * @return 
 */
void *inc_(void *xGlobalVariableVoidPointer)
{
	int *xGlobalVariableIntegerPointer = (int *) xGlobalVariableVoidPointer;
    
	for( int currentForIndex = 0; currentForIndex<100; currentForIndex++) 
	{
		// Enter critical region
        pthread_mutex_lock( xGlobalVariableMutexLocker );
        
		++( *xGlobalVariableIntegerPointer );
        
        // Leave critical region
        pthread_mutex_unlock( xGlobalVariableMutexLocker );
	}
    
	cout << "increment finished" << endl;
    
	return NULL;
}

void *dec_(void *xGlobalVariableVoidPointer)
{
    /* decrement x to 100 */
    int *xGlobalVariableIntegerPointer = (int *)xGlobalVariableVoidPointer;
    
	for( int currentForIndex = 0; currentForIndex<100; currentForIndex++ )
	{
		/* enter critical region */
		--(*ptr);
		/* leave critical region */
	}        
    cout << "decrement finished" << endl;
    return NULL;
}

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
