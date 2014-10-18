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
    T *dado;

    /**
     * Ponteiro para o elemento elemento anterior da árvore
     */
    NoBinario< T >* esquerda;

    /**
     * Ponteiro para o próximo elemento da árvore
     */
    NoBinario< T >* direita;

    /**
     * // Para ajudar nos herancas
     * 
     * @param arv
     * @return 
     */
    virtual NoBinario< T >* balanco_insere( NoBinario< T >* arv )
    {
        return arv;
    }
    ;

    /**
     * // Para ajudar nos herancas
     * 
     * @param arv
     * @return 
     */
    virtual NoBinario< T >* balanco_remove( NoBinario< T >* arv )
    {
        return arv;
    }
    ;

    /**
     * // No lugar dos prints 
     */
    std::vector< NoBinario< T > > elementos;

public:
    
    /**
     * Constrói uma nó de uma árvore binária caso algum dado seja fornecido.
     * Caso receba null, cria uma nova árvore.
     * 
     * @param dado a ser inserido na arvore
     */
    NoBinario< T >( const T& dado ) :
            dado( new T( dado ) ), esquerda( NULL ), direita( NULL )
    {
    }
    
    /**
     * Destrói o ponteiro para o elemento que esse nó ponta
     */
    ~NoBinario()
    {
        delete dado;
    }
    
    /**
     * Insere um nó em uma árvore de busca binária. Esta é um método chamado
     * recursivamente e ao inserir o novo nó, ele retorna o nó completo.
     * 
     * @param dado a ser inserido recursivamente pela árvore.
     * @param arvore binária a ter o elemento inserido, e durante a recursão, o
     *  nó a ter o elemento a ser inserido.
     * @return a árvore binária como o nó inserido na última chamada da 
     * recusão. Durante o processo recursivo, retorna os nós completos com 
     * o objeto inserido.
     */
    NoBinario< T >* inserir( const T& dado, NoBinario< T >* arv )
    {
        //caso o dado seja seja menor do que a raiz atual
        if( dado < arv->getDado() )
        {
            //faz a inserção a esquerda
            if( arv->getEsquerda() == NULL )
            {
                
            }
        }
    }
    
    /**
     *  Retorna um ponterio para o próximo elemento deste nó.
     * 
     * @return um ponteiro para o próximo elemento, NULL caso não exista.
     */
    NoBinario< T >* getDireita() const
    {
        return direita;
    }
    
    /**
     *  Retorna um ponterio para o elemento anterior deste nó.
     * 
     * @return um ponteiro para o elemento anterior deste nó, NULL caso 
     * não exista.
     */
    NoBinario< T >* getEsquerda() const
    {
        return esquerda;
    }
    
    /**
     * Retorna um ponteiro para o elemento que este nó armazena
     * 
     * @return um ponteiro para o elemento que este nó armazena
     */
    T getDado() const
    {
        return this->*dado;
    }
    
    /**
     * Altera qual o proximo elemento que este nó aponta
     * 
     * @param um ponteiro para o próximo nó
     */
    void setDireita( NoBinario< T >* next )
    {
        direita = next;
    }
    
    /**
     * Altera qual o elemento anterior que este nó aponta
     * 
     * @param um ponteiro para nó anterior
     */
    void setEsquerda( NoBinario< T >* previous )
    {
        esquerda = previous;
    }
    
    /**
     * Altera qual o dado que este nó referencia
     * 
     * @param o novo dado a ser referenciado por este nó
     */
    void setDado( const T& dado )
    {
        this->dado = dado;
    }
};

#endif /* NOBINARIO_HPP_ */
