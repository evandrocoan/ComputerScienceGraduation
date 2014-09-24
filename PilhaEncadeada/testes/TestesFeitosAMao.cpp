/*
 * teste
 *
 *  Created on: 17/09/2014
 *      Author: Professional
 */

/**
 * Classe que representa a lista encadeada
 */
#include "../PilhaEnc.hpp"

/**
 * Cuida das operações de entrada e saíde de texto
 */
#include <iostream>

using namespace std;

/**
 * Função que imprime na tela uma lista encadeada
 * 
 * @param lista endadeada a ser imprimida na tela
 */

class TestesFeitoAMao
{
    public:
        // função principal que realiza o teste
        void testePricipal()
        {
            cout << "Adicionando 0 na posição 0 e imprimindo a lista" << endl;
            ListaEnc< int > testeLista;
            testeLista.adicionaNaPosicao( 0, 0 );
            imprimirListaEncadeadaInteiros( testeLista );
            
            cout << "Adicionando 1 na posição 1 e imprimindo a lista" << endl;
            testeLista.adicionaNaPosicao( 1, 1 );
            imprimirListaEncadeadaInteiros( testeLista );
            
            cout << "Adicionando 2 na posição 0 e imprimindo a lista" << endl;
            testeLista.adicionaNaPosicao( 2, 0 );
            imprimirListaEncadeadaInteiros( testeLista );
            
            cout << "Adicionando 3 na posição 1 e imprimindo a lista" << endl;
            testeLista.adicionaNaPosicao( 3, 1 );
            imprimirListaEncadeadaInteiros( testeLista );
            
            cout << "Adicionando 4 na posição 4 e imprimindo a lista" << endl;
            testeLista.adicionaNaPosicao( 4, 4 );
            imprimirListaEncadeadaInteiros( testeLista );
        }
        
        void imprimirListaEncadeadaInteiros( ListaEnc< int > lista )
        {
            
            for( int i = 0; i < lista.tamanho(); i++ )
            {
                int temp = lista.retiraDaPosicao( i );
                cout << "Elemento: " << i << " = " << temp << endl;
                lista.adicionaNaPosicao( temp, i );
                
            }
            
            cout << endl;
        }
};
