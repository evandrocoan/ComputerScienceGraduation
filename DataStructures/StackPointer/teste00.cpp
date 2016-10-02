//============================================================================
/** Name        : teste00.cpp
 *
 * \author Charles B.L e Evandro C. (grupo 4)
 * \since 22/08/2014
 * \version 1.0
 *
 * 	Description : Programa para trabalhar com uma pilha  e fila de 
 *     valores (objetos)
 */
//============================================================================

#include <iostream>
#include "Pilha.hpp"
#include "Fila.hpp"

using namespace std;

int main()
{
	cout << "HELOOOO!! !!!";
   
   Fila<int> fila;
   
   cout << "Retorna se a fila está vazia, Esperado 1: " <<fila.filaVazia() 
           << endl;
   
   cout << "Retorna se a fila está cheia, Esperado 0: " <<fila.filaCheia() 
           << endl;
   
   fila.inclui( 5 );
   fila.inclui( 6 );
   
   cout << "Retorna se a fila está vazia, Esperado 0: " <<fila.filaVazia() 
        << endl;
   
   cout << "Retorna a posição do ultimo elemento, Esperado 1: "
           << fila.getUltimo() << endl;
   
   cout << "Retorna o primeiro elemento, Esperado 5: " << fila.retira()
           << endl;
   
   cout << "Retorna a posição do ultimo elemento, Esperado 0: "
        << fila.getUltimo() << endl;
   
   cout << "Retorna o primeiro elemento, Esperado 6: " << fila.retira()
        << endl;
   
   cout << "Retorna se a fila está vazia, Esperado 1: " <<fila.filaVazia() 
           << endl;

   cout << "Retorna a posição do ultimo elemento, Esperado -1: "
        << fila.getUltimo() << endl;
   
	return 0;
}
