//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/**
 * Arquivo que contém a implementação de uma árvore binária que utiliza 
 * templates.
 *
 * \authors Evandro  Coan, Charles Borges de Lima
 */

/**
 * Evita que esta classe seja incluida mais de uma vez em um arquivo
 */
#ifndef NOBINARIO_HPP_
#define NOBINARIO_HPP_

/**
 * Inclui as bibliotecas padrão utilizadas nesta implementação
 */
#include <cstdio>
#include <vector>

/**
 * Classe que representa um nó e uma arvore binaria simultaniamente.
 * Cada no é por si só uma arvore quando sua a informação que ele aponta é 
 * nula ele é o nó raíz.
 */
template< typename T >
class NoBinario
{
    
private:
    /**
     * Ponteiro para o tipo de informação que vai ser armazenada
     */
    T *info;

    /**
     * Ponteiro para o elemento elemento anterior da árvore
     */
    NoBinario< T >* esquerda;

    /**
     * Ponteiro para o próximo elemento da árvore
     */
    NoBinario< T >* direita;

public:
    
    NoBinario< T >( const T& dado ) :
            dado( new T( dado ) ), esquerda( NULL ), direita( NULL )
    {
    }
    
    /**
     * Destrói o ponteiro para o elemento que esse nó ponta
     */
    ~NoBinario()
    {
        delete info;
    }
    
    /**
     *  Retorna um ponterio para o próximo elemento deste nó.
     * 
     * @return um ponteiro para o próximo elemento, NULL caso não exista.
     */
    NoBinario< T >* getProximo() const
    {
        return direita;
    }
    
    /**
     *  Retorna um ponterio para o elemento anterior deste nó.
     * 
     * @return um ponteiro para o elemento anterior deste nó, NULL caso 
     * não exista.
     */
    NoBinario< T >* getAnterior() const
    {
        return esquerda;
    }
    
    /**
     * Retorna um ponteiro para o elemento que este nó armazena
     * 
     * @return um ponteiro para o elemento que este nó armazena
     */
    T getInfo() const
    {
        return *info;
    }
    
    /**
     * Altera qual o proximo elemento que este nó aponta
     * 
     * @param um ponteiro para o próximo nó
     */
    void setProximo( NoBinario< T >* next )
    {
        direita = next;
    }
    
    /**
     * Altera qual o elemento anterior que este nó aponta
     * 
     * @param um ponteiro para nó anterior
     */
    void setAnterior( NoBinario< T >* previous )
    {
        esquerda = previous;
    }
};

#endif /* NOBINARIO_HPP_ */
