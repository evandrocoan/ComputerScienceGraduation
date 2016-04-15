/*********************** Licensing *******************************************************
*
*  Copyright 2016 @ Evandro  Coan
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 3 of the License, or ( at
*  your option ) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*****************************************************************************************
*/

/**
 * Nome do aluno. Compile and link with -pthread.
 * 
 * @author Evandro  Coan
 * 
 */

 
#include <iostream>
#include <pthread.h>
#include <cstdlib>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
/* ... other includes ... */


/** This is to view internal program data while execution. Default value: 0
 * 
 * 0   - Disables this feature.
 * 1   - Normal debug.
 */
#define DEBUG_LEVEL 0

#define MAX_FOR_LOOPS_TO_INCREMENT_THE_GLOBAL_VARIABLE 100


#if DEBUG_LEVEL > 0
    #define DEBUG

/** Print like function for logging putting a new line at the end of string. Following explanations:
 * 
 * fprintf( stream, __VA_ARGS__ ); Print to the specified output stream the formatting args.
 * fprintf( stream, "\n" );        Print a new line.
 * fflush( stream );               Flushes the output stream to avoid double output over '>'.
 *                                   Example: './main > results.txt' would get doubled/... print.
 * } while( 0 )                    To allow to use ';' semicolon over the macro statement use and
 *                                   still to be able to use it within an unbraced if statement.
 */
#define DEBUGGER( stream, ... ) \
{ \
    fprintf( stream, __VA_ARGS__ ); \
    fprintf( stream, "\n" ); \
    fflush( stream ); \
} while( 0 )

#else
    #define DEBUGGER( stream, ... )
    
#endif


/* declare whenever global variables you need to synchronize pthreads using posix semaphores */


// Functions prototypes
void *child(void *)


using namespace std;


/** 
 * Para pensar e para responder no código
 *     
 * 
 * Os semáforos podem ser variáveis locais?
 *     
 * 
 * Todas as threads "child" terminam? Justifique.
 *     
 * 
 * E se no cesto couber uma única bola, como na versão original do problema?
 *     
 * 
 * Há algum erro de programação para que as threads não terminem?
 *     
 * 
 * O que poderia ser feito para detectar que um evento nunca ocorrerá e fazer com que o programa
 * finalize com sucesso, encerrando todas suas threads?
 *     
 * 
 */
int main()
{
    cout << "The kindengarten is open" << endl;
    
    /* declare local variable, such as threads */
    
    
    /* init semaphores to synchronize the threads */
    
    
    /* create 7 threads for the children, passing to each one a different number (child 0 to 6) */
    
    
    /* wait for all children to finish */
    
    
    cout << "The kindengarten is closed" << endl;
    
    return 0;
    
    /** Respostas das perguntas "para pensar":
     *
     * @see function main() documentation.
     */
}

/**
 * 
 */
void *child(void *void_ptr)
{
    unsigned short *childNum = (unsigned short *)void_ptr;
    
    for (unsigned short i=1; i<=5; i++) 
    {
        cout << "Child " << *childNum << " wants to play with the ball for the " << i << "th time" << endl;
        
        /* if the child has no ball, need to take one from the basket if there is one, or will wait until there is a ball in the basket */
            cout << " Child " << *childNum << " wants to take a ball from the basket" << endl;
        
        /* once the child has a ball, he/she starts to play */
        cout << "  Child " << *childNum << " is playing with the ball" << endl;
        /* play with the ball for 1 second */
        
        
        cout << "  Child " << *childNum << " wants to leave the ball in the basket" << endl;
        
        /* when the child is tired of playing, he/she has to drop the ball into the basket, if there is room for it (basket holds only 3 balls), or will wait until another hild to take a ball */
        cout << " Child " << *childNum << " has droped the ball in the basket" << endl;
        
    }
    
    cout << "Child " << *childNum << " will no longer play" << endl;  
    
    /* exit the thread*/
    
}