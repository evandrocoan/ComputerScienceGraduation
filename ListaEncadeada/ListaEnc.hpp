/*
 /*
 * ListaEnc.hpp
 *
 *  Created on: 13/09/2014
 *      Author: Carcara
 */
//MEUUUUUUUUU ULTIMOOOOOOOOO))))
#ifndef LISTAENC_HPP_
#define LISTAENC_HPP_
#include <stdio.h>
#include <stdlib.h>
#include "Elemento.hpp"
#define ERROLISTACHEIA -1
#define ERROLISTAVAZIA -2
#define ERRODEPOSICAO -3

/**
 * Header de 
 */
template< typename T >
class ListaEnc
{
    private:
        Elemento< T >* head; //ponteiro para o primeiro elemento da lista
        int size; //numero de elementos da lista
    public:
        
        /**
         * Construtor padrão que cria uma lista encadeada vazia
         */
        ListaEnc();
        ~ListaEnc();
        //****************************
        ListaEnc< T >* criaLista();
        //****************************
        // inicio
        void adicionaNoInicio( const T& dado );
        T retiraDoInicio();
        int eliminaDoInicio();
        // posicao
        void adicionaNaPosicao( const T& dado, int pos );
        int posicao( const T& dado ) const;
        T* posicaoMem( const T& dado ) const;
        bool contem( const T& dado );
        T retiraDaPosicao( int pos );
        //fim
        void adiciona( const T& dado );
        T retira();
        // especifico
        T retiraEspecifico( const T& dado );
        void adicionaEmOrdem( const T& data );
        bool listaVazia() const;
        bool igual( T dado1, T dado2 );
        bool maior( T dado1, T dado2 );
        bool menor( T dado1, T dado2 );
        void destroiLista();
        bool posicaoInvalida( int p );
};
//-----------------------------------------------------------------
template< typename T >
ListaEnc< T >::ListaEnc()
{
    head = NULL;
    size = 0;
}

//-----------------------------------------------------------------
template< typename T >
ListaEnc< T >::~ListaEnc()
{
    destroiLista(); //***error: expected primary-expression before ‘;’ token
}
//-----------------------------------------------------------------
template< typename T >
void ListaEnc< T >::adicionaNoInicio( const T& dado )
{
    Elemento< T > *novo;
    
    novo = new Elemento< T >( dado, head );
    if ( novo == NULL )
        throw ERROLISTACHEIA;
    else
    {
        //novo->setProximo(head);  //novo->setProximo(dados); poderia ser feito diretamente em Elemento<T>(dado,head);
        head = novo;
        size++;
    }
}
//-------------------------------------------------------------------

//-----------------------------------------------------------------
template< typename T >
T ListaEnc< T >::retiraDoInicio()
{
    if ( listaVazia() )
    {
        return NULL;
    }
    Elemento< T >* saiu = head;
    T volta = saiu->getInfo();
    head = saiu->getProximo();
    size--;
    delete saiu;
    return volta;
}

//-----------------------------------------------------------------
template< typename T >
int ListaEnc< T >::eliminaDoInicio()
{
    if ( listaVazia() )
    {
        throw ERROLISTAVAZIA;
    }
    Elemento< T >* saiu = head;
    head = saiu->getProximo();
    size--;
    delete saiu;
    return size;
}

/**
 * Adiciona um elemento em uma posição específica.
 * 
 * @param dado a ser adicionado
 * @param pos a posição para adicionar o dado
 * @throw erro de posição se a posição é inexistente
 */
template< typename T >
void ListaEnc< T >::adicionaNaPosicao( const T& dado, int pos )
{
    // verifica se está tentando adicionar em uma posição inexistente
    if ( pos > size + 1 )
    {
        throw ERRODEPOSICAO;
    } else
    {
        // caso seja a primeia posição, chama o método que manipula a cabeça
        if ( pos == 0 )
        {
            return adicionaNoInicio( dado );
        }

        Elemento< T >* novo = new Elemento< T >( dado, 0 );
        if ( novo == NULL )
        {
            throw ERROLISTACHEIA;
        }
        
        
        Elemento< T >* anterior = head;
        for ( int i = 0; i < pos - 2; i++ )
        {
            anterior = anterior->getProximo();
        }
        
        novo->setProximo( anterior->getProximo() );
        anterior->setProximo( novo );
        size++;
    }
}
//-----------------------------------------------------------------
template< typename T >
int ListaEnc< T >::posicao( const T& dado ) const
{
    if ( listaVazia() )
    {
        throw -4; //ExcecaoListaVazia();
    }
    Elemento< T >* atual = head;
    for ( int i = 0; i < size; i++ )
    {
        if ( dado == atual->getInfo() )
        {
            return i;
        }
        atual = atual->getProximo();
    }
    throw -5; //ExcecaoDadoNaoEncontrado();
}
//-----------------------------------------------------------------
template< typename T >
T* ListaEnc< T >::posicaoMem( const T& dado ) const
{
    if ( listaVazia() )
    {
        throw -1; //ExcecaoListaVazia();
    }
    int posicao = posicao( dado );
    Elemento< T >* atual = head;
    for ( int i = 0; i < posicao; i++ )
    {
        atual = atual->getProximo();
    }
    return atual->getInfo();
}
//-----------------------------------------------------------------
template< typename T >
bool ListaEnc< T >::contem( const T& dado )
{
    if ( listaVazia() )
    {
        throw -2; //ExcecaoListaVazia();
    }
    Elemento< T >* atual = head;
    for ( int i = 0; i < size; i++ )
    {
        if ( igual( dado, atual->getInfo() ) )
        {
            return true;
        }
        atual = atual->getProximo();
    }
    return false;
}
//-----------------------------------------------------------------
template< typename T >
T ListaEnc< T >::retiraDaPosicao( int posicao )
{
    if ( posicaoInvalida( posicao ) )
    {
        throw ERRODEPOSICAO;
    }
    if ( posicao == 0 )
    {
        return retiraDoInicio();
    }
    Elemento< T >* anterior = head;
    for ( int i = 0; i < posicao - 2; i++ )
    {
        anterior = anterior->getProximo();
    }
    Elemento< T >* eliminar = anterior->getProximo();
    T volta = eliminar->getInfo();
    anterior->setProximo( eliminar->getProximo() );
    size--;
    delete eliminar;
    return volta;
}
//-----------------------------------------------------------------
template< typename T >
void ListaEnc< T >::adiciona( const T& dado )
{
    return adicionaNaPosicao( dado, size );
}
//-----------------------------------------------------------------
template< typename T >
T ListaEnc< T >::retira()
{
    return retiraDaPosicao( size );
}
//-----------------------------------------------------------------
template< typename T >
T ListaEnc< T >::retiraEspecifico( const T& dado )
{
    if ( listaVazia() )
    {
        throw -3; //ExcecaoListaVazia();
    }
    return retiraDaPosicao( posicao( dado ) + 1 );
}
//-----------------------------------------------------------------
template< typename T >
void ListaEnc< T >::adicionaEmOrdem( const T& dado )
{
    if ( listaVazia() )
    {
        return adicionaNoInicio( dado );
    }
    Elemento< T >* atual = head;
    int posicao = 1;
    while( atual->getProximo() != 0 && maior( dado, atual->getInfo() ) )
    {
        atual = atual->getProximo();
        posicao++;
    }
    if ( maior( dado, atual->getInfo() ) )
    {
        return adicionaNaPosicao( dado, posicao + 1 );
    }
    adicionaNaPosicao( dado, posicao );
}

//-----------------------------------------------------------------
template< typename T >
bool ListaEnc< T >::listaVazia() const
{
    if ( size == 0 )
        return true;
    return false;
}
//-----------------------------------------------------------------
template< typename T >
bool ListaEnc< T >::igual( T dado1, T dado2 )
{
    return dado1 == dado2;
}
//-----------------------------------------------------------------
template< typename T >
bool ListaEnc< T >::maior( T dado1, T dado2 )
{
    return dado2 < dado1;
}
//-----------------------------------------------------------------
template< typename T >
bool ListaEnc< T >::menor( T dado1, T dado2 )
{
    return dado1 < dado2;
}
//-----------------------------------------------------------------
template< typename T >
void ListaEnc< T >::destroiLista()
{
    Elemento< T >* atual;
    while( head )
    {
        atual = head;
        head = head->getProximo();
        delete atual;
    }
    size = 0;
}
//------------------------------------------------------------------
template< typename T >
bool ListaEnc< T >::posicaoInvalida( int p )
{
    return ( p >= size + 1 || p < 0 );
}
//******************************************************************
template< typename T >
ListaEnc< T >* ListaEnc< T >::criaLista()
{
    ListaEnc* aLista;
    aLista = new ListaEnc();
    if ( aLista != NULL )
    {
        aLista->size = 0;
        aLista->head = NULL;
    }
    return aLista;
}
//******************************************************************
//-----------------------------------------------------------------
#endif /* LISTAENC_HPP_ */
