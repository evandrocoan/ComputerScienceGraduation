/**
 * Arquivo que contém testes aplicados à uma árvore bináira.
 */

/**
 * Inclui os métodos utilizados nos testes
 */
#include "NoBinario.hpp"
#include <stdio.h>
#include <iostream>

/**
 * Declara o uso do espaço de trabalho stdio
 */
using namespace std;

/**
 * Protótipos das funções utilizadas
 */
void testesCout();

/**
 * Função principal que realiza a chamada dos testes a serem executados.
 * 
 * @return o código de retorno do estado da execuçãodo programa
 */
int main()
{
    testesCout();
    
    return 0;
}

/**
 * Função que realiza teste utilizando cout
 */
void testesCout()
{
    
    //clrscr();
    NoBinario< int > arvore(1);
    
    cout << "\n\t\tADDING NEW NODE" << endl;
    cout << "\t\t:::::::::::::\n" << endl;
    cout << "int = 1";
    //arvore.insert( 1, root );
    cout << "\nThe new value have been added to your tree successfully\n"
         << endl;

    cout << "Sorry! wrong input\n" << endl;

}
