//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/**
 * FilaEnc.hpp
 *
 * \authors Evandro  Coan, Charles Borges de Lima
 */

/**
 * Evita incluir esta classe mais de uma vez no processo de compilação
 */
#ifndef FILAENC_HPP_
#define FILAENC_HPP_

/**
 * Bibliotecas utilizadas
 */
#include <stdio.h>
#include <stdlib.h>
#include "Elemento.hpp"

/**
 * Códigos de erros definidos
 */
#define ERROFILACHEIA -1
#define ERROFILAVAZIA -2
#define ERRODEPOSICAO -3

/**
 * Header para um fila encadeada
 */
template< typename T >
class FilaEnc
{
    
private:
    /**
     * Ponteiro para o primeiro elemento da fila
     */
    Elemento< T >* head;

    /**
     * Número de elementos da fila
     */
    int size;

    /**
     * Adiciona um dado recebido como parâmetro no início da fila
     *
     * @param dado
     */
    void adicionaNoInicio( const T& dado );

    /**
     * Informa se uma determinada posição válida.
     *
     * @param a posição a ser verificada
     * @return true se a posição é válida, false caso contrário
     */
    bool posicaoInvalida( int p );

    /**
     * Elimina um elemento de uma certa posição nesta fila encadeada.
     *
     * @param a posição do elemento a ser retirado
     * @return o elemento retirado da posição
     */
    T retiraDaPosicao( int pos );

    /**
     * Retira o elemento da cabeça da fila encadeada
     *
     * @return o elemento na cabeça da fila encadeada
     */
    T retiraDoInicio();

public:
    
    /**
     * Construtor padrão que cria uma fila encadeada vazia
     */
    FilaEnc();

    /**
     * Destrutor padrão que destrói os dados que aponta a fila encadeada
     */
    ~FilaEnc();

    /**
     * Adiciona emfila um elemento na fila.
     *
     * @param dado a ser adicionado
     */
    void inclui( const T& dado );

    /**
     * Retira o elemento pertencente ao início da fila encadeada
     *
     * @return o dado retirado da fila encadeada
     */
    T retira();

    /**
     * Destrói todos os ponteiros da fila e ajusta seu tamanho para 0
     */
    void limparFila();

    /**
     * Informa se a fila encadeada está vazia
     *
     * @return true se a fila encadeada está vazia, false caso contrário
     */
    bool filaVazia();

    /**
     * Retorna o elemento que se encontra no final da fila sem retirá-lo
     * da fila.
     *
     * @return o elemento no final da fila
     */
    T ultimo();
};

/**
 * \copydoc FilaEnc< T >::FilaEnc()
 */
template< typename T >
FilaEnc< T >::FilaEnc()
{
    head = NULL;
    size = 0;
}

/**
 * \copydoc FilaEnc< T >::~FilaEnc()
 */
template< typename T >
FilaEnc< T >::~FilaEnc()
{
    this->limparFila();
}

/**
 * \copydoc FilaEnc< T >::posicaoInvalida( int p )
 */
template< typename T >
bool FilaEnc< T >::posicaoInvalida( int p )
{
    return ( p >= size + 1 || p < 0 );
}

/**
 * \copydoc FilaEnc< T >::inclui( const T& dado, int pos )
 */
template< typename T >
void FilaEnc< T >::inclui( const T& dado )
{
    int pos = this->size;
    
    // verifica se está tentando adicionar em uma posição inexistente
    if( posicaoInvalida( pos ) )
    {
        throw ERRODEPOSICAO;
    }
    
    // caso seja a primeia posição, chama o método que manipula a cabeça
    if( pos == 0 )
    {
        return adicionaNoInicio( dado );
    }
    
    // caso seja a última posição, chama o método que manipula a calda
    //    if( pos == size )
    //    {
    //        return adiciona
    //    }
    
    // aloca um elemento no inicio da fila
    Elemento< T >* novo = new Elemento< T >( dado, 0 );
    
    // verifica se ela está cheia
    if( novo == 0 )
    {
        throw ERROFILACHEIA;
    }
    
    // salva o ponterio da cabeça da fila
    Elemento< T >* anterior = head;
    
    //
    for( int i = 0; i < pos - 1; i++ )
    {
        anterior = anterior->getProximo();
    }
    
    // 
    novo->setProximo( anterior->getProximo() );
    
    anterior->setProximo( novo );
    size++;
}

/**
 * \copydoc FilaEnc< T >::retira()
 */
template< typename T >
T FilaEnc< T >::retira()
{
    return retiraDaPosicao( 0 );
}

/**
 * \copydoc FilaEnc< T >::retiraDaPosicao( int posicao )
 */
template< typename T >
T FilaEnc< T >::retiraDaPosicao( int posicao )
{
    //Variável auxiliar para elemento.
    //Elemento< T >* anterior, eliminar;
    
    if( posicaoInvalida( posicao ) )
    {
        throw ERRODEPOSICAO;
    } else
    {
        if( posicao == 0 )
        {
            return retiraDoInicio();
        } else
        {
            Elemento< T >* anterior = head;
            
            for( int i = 0; i < posicao - 1; i++ )
            {
                anterior = anterior->getProximo();
            }
            Elemento< T >* eliminar = anterior->getProximo();
            
            //Variável auxiliar para o dado retornado
            T volta = eliminar->getInfo();
            anterior->setProximo( eliminar->getProximo() );
            size--;
            delete eliminar;
            return volta;
        }
    }
}

/**
 * \copydoc FilaEnc< T >::retiraDoInicio()
 */
template< typename T >
T FilaEnc< T >::retiraDoInicio()
{
    if( filaVazia() )
    {
        throw ERROFILACHEIA;
    }
    Elemento< T >* saiu = head;
    T volta = saiu->getInfo();
    head = saiu->getProximo();
    size--;
    delete saiu;
    return volta;
}

/**
 * \copydoc FilaEnc< T >::topo()
 */
template< typename T >
T FilaEnc< T >::ultimo()
{
    T elemento = this->retiraDaPosicao( size - 1);
    
    this->inclui( elemento );
    
    return elemento;
}

/**
 * \copydoc FilaEnc< T >::filaVazia() const
 */
template< typename T >
bool FilaEnc< T >::filaVazia()
{
    if( size == 0 )
        return true;
    return false;
}

/**
 * \copydoc FilaEnc< T >::adicionaNoInicio( const T& dado )
 */
template< typename T >
void FilaEnc< T >::adicionaNoInicio( const T& dado )
{
    Elemento< T > *novo = new Elemento< T >( dado, head );
    
    if( novo == NULL )
        throw ERROFILACHEIA;
    else
    {
        //poderia ser feito diretamente em Elemento<T>(dado,head);
        //novo->setProximo(dados); 
        //novo->setProximo(head);  
        head = novo;
        size++;
    }
}

/**
 * \copydoc FilaEnc< T >::destroiFila()
 */
template< typename T >
void FilaEnc< T >::limparFila()
{
    while( head )
    {
        Elemento< T > * atual = head;
        
        head = head->getProximo();
        
        delete atual;
        
        this->size--;
        
        // evitar acessar um null pointer
        if( atual != 0 )
        {
            atual = atual->getProximo();
        }
    }
}

#endif /* FILAENC_HPP_ */
