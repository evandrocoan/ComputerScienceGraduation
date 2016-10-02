//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/**
 * PilhaEnc.hpp
 * 
 * \authors Evandro  Coan, Charles Borges de Lima
 */

/**
 * Evita incluir esta classe mais de uma vez no processo de compilação
 */
#ifndef PILHAENC_HPP_
#define PILHAENC_HPP_

/**
 * Bibliotecas utilizadas
 */
#include <stdio.h>
#include <stdlib.h>
#include "Elemento.hpp"

/**
 * Códigos de erros definidos
 */
#define ERROPILHACHEIA -1
#define ERROPILHAVAZIA -2
#define ERRODEPOSICAO -3

/**
 * Header para um pilha encadeada
 */
template< typename T >
class PilhaEnc
{
    
private:
    /**
     * Ponteiro para o primeiro elemento da pilha
     */
    Elemento< T >* head;

    /**
     * Número de elementos da pilha
     */
    int size;

    /**
     * Adiciona um dado recebido como parâmetro no início da pilha
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
     * Elimina um elemento de uma certa posição nesta pilha encadeada.
     * 
     * @param a posição do elemento a ser retirado
     * @return o elemento retirado da posição
     */
    T retiraDaPosicao( int pos );

    /**
     * Retira o elemento da cabeça da pilha encadeada
     * 
     * @return o elemento na cabeça da pilha encadeada
     */
    T retiraDoInicio();

public:
    
    /**
     * Construtor padrão que cria uma pilha encadeada vazia
     */
    PilhaEnc();

    /**
     * Destrutor padrão que destrói os dados que aponta a pilha encadeada
     */
    ~PilhaEnc();

    /**
     * Adiciona empilha um elemento na pilha.
     * 
     * @param dado a ser adicionado
     */
    void empilha( const T& dado );

    /**
     * Retira o elemento pertencente ao final da pilha encadeada
     * 
     * @return o dado retirado da pilha encadeada
     */
    T desempilha();

    /**
     * Destrói todos os ponteiros da pilha e ajusta seu tamanho para 0
     */
    void limparPilha();

    /**
     * Informa se a pilha encadeada está vazia
     * 
     * @return true se a pilha encadeada está vazia, false caso contrário
     */
    bool PilhaVazia();

    /**
     * Retorna o elemento que se encontra no topo da pilha sem retirá-lo 
     * da pilha.
     * 
     * @return o elemento no topo da pilha
     */
    T topo();
};

/**
 * \copydoc PilhaEnc< T >::PilhaEnc()
 */
template< typename T >
PilhaEnc< T >::PilhaEnc()
{
    head = NULL;
    size = 0;
}

/**
 * \copydoc PilhaEnc< T >::~PilhaEnc()
 */
template< typename T >
PilhaEnc< T >::~PilhaEnc()
{
    this->limparPilha();
}

/**
 * \copydoc PilhaEnc< T >::posicaoInvalida( int p )
 */
template< typename T >
bool PilhaEnc< T >::posicaoInvalida( int p )
{
    return ( p >= size + 1 || p < 0 );
}

/**
 * \copydoc PilhaEnc< T >::empilha( const T& dado, int pos )
 */
template< typename T >
void PilhaEnc< T >::empilha( const T& dado )
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
    
    // aloca um elemento no inicio da pilha
    Elemento< T >* novo = new Elemento< T >( dado, 0 );
    
    // verifica se ela está cheia
    if( novo == 0 )
    {
        throw ERROPILHACHEIA;
    }
    
    // salva o ponterio da cabeça da pilha
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
 * \copydoc PilhaEnc< T >::retira()
 */
template< typename T >
T PilhaEnc< T >::desempilha()
{
    return retiraDaPosicao( size - 1 );
}

/**
 * \copydoc PilhaEnc< T >::retiraDaPosicao( int posicao )
 */
template< typename T >
T PilhaEnc< T >::retiraDaPosicao( int posicao )
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
 * \copydoc PilhaEnc< T >::retiraDoInicio()
 */
template< typename T >
T PilhaEnc< T >::retiraDoInicio()
{
    if( PilhaVazia() )
    {
        throw ERROPILHACHEIA;
    }
    Elemento< T >* saiu = head;
    T volta = saiu->getInfo();
    head = saiu->getProximo();
    size--;
    delete saiu;
    return volta;
}

/**
 * \copydoc PilhaEnc< T >::topo()
 */
template< typename T >
T PilhaEnc< T >::topo()
{
    T elemento = this->desempilha();
    
    this->empilha( elemento );
    
    return elemento;
}

/**
 * \copydoc PilhaEnc< T >::PilhaVazia() const
 */
template< typename T >
bool PilhaEnc< T >::PilhaVazia()
{
    if( size == 0 )
        return true;
    return false;
}

/**
 * \copydoc PilhaEnc< T >::adicionaNoInicio( const T& dado )
 */
template< typename T >
void PilhaEnc< T >::adicionaNoInicio( const T& dado )
{
    Elemento< T > *novo = new Elemento< T >( dado, head );

    if( novo == NULL )
	throw ERROPILHACHEIA;
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
 * \copydoc PilhaEnc< T >::destroiPilha()
 */
template< typename T >
void PilhaEnc< T >::limparPilha()
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

#endif /* PILHAENC_HPP_ */
