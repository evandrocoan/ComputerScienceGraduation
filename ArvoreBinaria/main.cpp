/**
 * Arquivo que contém testes aplicados à uma árvore bináira.
 */

/*
 * #ifndef NOBINARIO_H_
 * #define NOBINARIO_H_
 * 
 * #include <cstdio>
 * #include <vector>
 * 
 * template<typename T>
 * class NoBinario {
 * 
 * private:
 *     T* dado;
 *     NoBinario<T>* esquerda;
 *     NoBinario<T>* direita;
 * 
 *     // Para  ajudar nos herancas
 *     virtual NoBinario<T>* balanco_insere(NoBinario<T>* arv) { return arv; }; 
 * 
 *     // P*ar a  ajudar nos herancas
 *     virtual NoBinario<T>* balanco_remove(NoBinario<T>* arv) { return arv; }; 
 * 
 *     std::vector<NoBinario<T> > elementos; // No lugar dos prints
 * 
 * public:
 *     NoBinario<T>(const T& dado): dado(new T(dado)), esquerda(NULL), 
 *                                                     direita(NULL) {} 
 *     virtual ~NoBinario<T>() {}
 *     T* getDado(){}
 *     NoBinario<T>* getElementos(){}
 *     T* busca(const T& dado, NoBinario<T>* arv) {}
 *     NoBinario<T>* inserir(const T& dado, NoBinario<T>* arv) {}
 *     NoBinario<T>* remover(NoBinario<T>* arv, const T& dado) {}
 *     NoBinario<T>* minimo(NoBinario<T>* nodo) {}
 *     void preOrdem(NoBinario<T>* nodo) {}
 *     void emOrdem(NoBinario<T>* nodo) {}
 *     void posOrdem(NoBinario<T>* nodo) {}
 * };
 * 
 * #endif /* NOBINARIO_HPP_ */

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
