//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/**
 * ListaCirc.hpp
 * 
 * \authors Evandro  Coan, Charles Borges de Lima
 */

/**
 * Evita incluir esta classe mais de uma vez no processo de compilação
 */
#ifndef LISTACIRC_HPP_
#define LISTACIRC_HPP_

/**
 * Bibliotecas utilizadas
 */
#include <stdio.h>
#include <stdlib.h>
#include "Elemento.hpp"

/**
 * Códigos de erros definidos
 */
#define ERROLISTACHEIA -1
#define ERROLISTAVAZIA -2
#define ERRODEPOSICAO -3

/**
 * Header para um lista circadeada
 */
template< typename T >
class ListaCirc
{
    
private:
    /**
     * Ponteiro para o primeiro elemento da lista
     */
    Elemento< T >* head;

    /**
     * Número de elementos da lista
     */
    int size;

public:
    
    /**
     * Construtor padrão que cria uma lista circadeada vazia
     */
    ListaCirc();

    /**
     * Destrutor padrão que destrói os dados que aponta a lista circadeada
     */
    ~ListaCirc();

    /**
     * Informa o tamanho atual da lista circadeada. 
     * 
     * @return um inteiro informando o tamnha da lista circadeada
     */
    int tamanho();

    /*
     // ****************************
     ListaCirc< T >* criaLista();
     // ****************************
     */

    /**
     * Adiciona um dado recebido como parâmetro no início da lista
     * 
     * @param dado
     */
    void adicionaNoInicio( const T& dado );

    /**
     * Retira o elemento da cabeça da lista circadeada
     * 
     * @return o elemento na cabeça da lista circadeada
     */
    T retiraDoInicio();

    /**
     * Deleta o elemento no ínicio na cabeça da lista circadeada
     * 
     * @return o tamanho da lista circadeada após a remoção da cabeça
     */
    int eliminaDoInicio();

    /**
     * Adiciona um elemento em uma posição específica.
     * 
     * @param dado a ser adicionado
     * @param pos a posição para adicionar o dado
     * @throw erro de posição se a posição é inexistente
     */
    void adicionaNaPosicao( const T& dado, int pos );

    /**
     * Retorna a posição de um elemento fornecido como parâmetro. 
     * A posição nesta lista circadeada começa em 0, para o primeiro 
     * elemento.
     * 
     * @param dado a ter a posição circontrada
     * @return a posição do dado circontrado com um inteiro
     */
    int posicao( const T& dado ) const;

    /**
     * Descobre qual o ponteiro para um determinado elemento na lista.
     * 
     * @param dado a ser procurado um ponteiro
     * @return o ponteiro para o dado passado como parâmetro
     */
    T* posicaoMem( const T& dado ) const;

    /**
     * Informa se a lista circadeada contém o dado passado como parâmetro.
     * 
     * @param dado a ser circontrado
     * @return true se a lista circadeada contém o dada passado como 
     * parâmetro, false caso contrário.
     */
    bool contem( const T& dado );

    /**
     * Elimina um elemento de uma certa posição nesta lista circadeada.
     * 
     * @param a posição do elemento a ser retirado
     * @return o elemento retirado da posição
     */
    T retiraDaPosicao( int pos );

    /**
     * Adiciona um elemento no final da lista circadeada
     * 
     * @param dado a ser adicionado
     */
    void adiciona( const T& dado );

    /**
     * Retira o elemento pertcircente ao final da lista circadeada
     * 
     * @return o dado retirado da lista circadeada
     */
    T retira();

    /**
     * Retira a primeiro ocorrência do dado fornecidado como parâmetro
     * 
     * @param dado a ter a primeira ocorrência retirada
     * @return a primeira ocorrência do dado retirado
     */
    T retiraEspecifico( const T& dado );

    /**
     * Adiciona um dado a lista circadeada, na ordem definida pelo tipo
     * de objeto que essa lista circadeada reprensenta
     * 
     * @param dado a ser adicionada em ordem
     */
    void adicionaEmOrdem( const T& data );

    /**
     * Informa se a lista circadeada está vazia
     * 
     * @return true se a lista circadeada está vazia, false caso contrário
     */
    bool listaVazia() const;

    /**
     * Informa se dois dados são iguais. Utiliza o operador de igualdade 
     * do tipo do objeto que essa lista circadeada representa para 
     * determinar se eles são iguais.
     * 
     * @param dado1 a ser comparado
     * @param dado2 a ser comparado
     * @return true se os parametrôs são iguais, false caso contrário
     */
    bool igual( T dado1, T dado2 );

    /**
     * Informa se o primeiro parâmetro é maior que o segundo. Utiliza o 
     * operador de igualdade do tipo do objeto que essa lista circadeada 
     * representa para determinar se eles são iguais.
     * 
     * @param dado1 a ser comparado
     * @param dado2 a ser comparado
     * @return true se o primeiro parâmetro é maior que o segundo, 
     * false caso contrário
     */
    bool maior( T dado1, T dado2 );

    /**
     * Informa se o primeiro parâmetro é menor que o segundo. Utiliza o 
     * operador de igualdade do tipo do objeto que essa lista circadeada 
     * representa para determinar se eles são iguais.
     * 
     * @param dado1 a ser comparado
     * @param dado2 a ser comparado
     * @return true se o primeiro parâmetro é menor que o segundo, 
     * false caso contrário
     */
    bool menor( T dado1, T dado2 );

    /**
     * Destrói todos os ponteiros da lista e ajusta seu tamanho para 0
     */
    void destroiLista();

    /**
     * Informa se uma determinada posição válida.
     * 
     * @param a posição a ser verificada
     * @return true se a posição é válida, false caso contrário
     */
    bool posicaoInvalida( int p );
};

/**
 * \copydoc ListaCirc< T >::ListaCirc()
 */
template< typename T >
ListaCirc< T >::ListaCirc()
{
    // cria-se o nodo sentinela
    head = new Elemento< T >( NULL, NULL );
    size = 0;
}

/**
 * \copydoc ListaCirc< T >::~ListaCirc()
 */
template< typename T >
ListaCirc< T >::~ListaCirc()
{
    this->destroiLista( );
}

/**
 * \copydoc ListaCirc< T >::adicionaNoInicio( const T& dado )
 */
template< typename T >
void ListaCirc< T >::adicionaNoInicio( const T& dado )
{
    // quando se adiciona pela primeira vez
    if( this->size == 0 )
    {
        Elemento< T > *novo = new Elemento< T >( dado, head );
        
        head->setProximo( novo );
        
        size++;
    } else
    {
        // salva o segundo (futuro terceiro) dado da lista
        // para emendar com novo segundo dado
        Elemento< T >* segundoDado = head->getProximo( );
        
        // aloca um elemento no inicio da lista
        Elemento< T >* novoSegundoDado = new Elemento< T >( dado, segundoDado );
        
        // verifica se ela está cheia
        if( novoSegundoDado == 0 )
        {
            throw ERROLISTACHEIA;
        }
        
        // agora faz o head apontar para o novo segundo dado
        head->setProximo( novoSegundoDado );
        
        size++;
    }
}

/**
 * \copydoc ListaCirc< T >::retiraDoInicio()
 */
template< typename T >
T ListaCirc< T >::retiraDoInicio()
{
    if( listaVazia( ) )
    {
        throw ERROLISTACHEIA;
    }
    Elemento< T >* saiu = head->getProximo();
    T volta = saiu->getInfo( );
    head->setProximo( saiu->getProximo( ) );
    size--;
    delete saiu;
    return volta;
}

/**
 * \copydoc ListaCirc< T >::eliminaDoInicio()
 */
template< typename T >
int ListaCirc< T >::eliminaDoInicio()
{
    if( listaVazia( ) )
    {
        throw ERROLISTAVAZIA;
    }
    Elemento< T >* saiu = head->getProximo();
    head->setProximo( saiu->getProximo( ) );
    size--;
    delete saiu;
    return size;
}

/**
 * \copydoc ListaCirc< T >::tamanho()
 */
template< typename T >
int ListaCirc< T >::tamanho()
{
    return this->size;
}

/**
 * \copydoc ListaCirc< T >::adicionaNaPosicao( const T& dado, int pos )
 */
template< typename T >
void ListaCirc< T >::adicionaNaPosicao( const T& dado, int pos )
{
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
    
    // aloca um elemento no inicio da lista
    Elemento< T >* novo = new Elemento< T >( dado, 0 );
    
    // verifica se ela está cheia
    if( novo == 0 )
    {
        throw ERROLISTACHEIA;
    }
    
    // salva o ponterio da cabeça da lista
    Elemento< T >* anterior = head->getProximo();
    
    // navega até o penúltimo elemento
    // pois o último aponta para NULL
    // caso se navegar até o último elemento, e ele apontar para o elemento
    // sentinela, então, 
    for( int i = 0; i < pos - 1; i++ )
    {
        anterior = anterior->getProximo( );
    }
    
    // 
    novo->setProximo( anterior->getProximo( ) );
    
    anterior->setProximo( novo );
    size++;
}

/**
 * \copydoc ListaCirc< T >::posicao( const T& dado ) const
 */
template< typename T >
int ListaCirc< T >::posicao( const T& dado ) const
{
    if( listaVazia( ) )
    {
        throw -4; //ExcecaoListaVazia();
    }
    Elemento< T >* atual = head->getProximo();
    for( int i = 0; i < size; i++ )
    {
        if( dado == atual->getInfo( ) )
        {
            return i;
        }
        atual = atual->getProximo( );
    }
    throw -5; //ExcecaoDadoNaoCircontrado();
}

/**
 * \copydoc ListaCirc< T >::posicaoMem( const T& dado ) const
 */
template< typename T >
T* ListaCirc< T >::posicaoMem( const T& dado ) const
{
    if( listaVazia( ) )
    {
        throw -1; //ExcecaoListaVazia();
    }
    int posicao = posicao( dado );
    Elemento< T >* atual = head->getProximo();
    for( int i = 0; i < posicao; i++ )
    {
        atual = atual->getProximo( );
    }
    return atual->getInfo( );
}

/**
 * \copydoc ListaCirc< T >::contem( const T& dado )
 */
template< typename T >
bool ListaCirc< T >::contem( const T& dado )
{
    if( listaVazia( ) )
    {
        throw -2; //ExcecaoListaVazia();
    }
    Elemento< T >* atual = head->getProximo();
    for( int i = 0; i < size; i++ )
    {
        if( igual( dado, atual->getInfo( ) ) )
        {
            return true;
        }
        atual = atual->getProximo( );
    }
    return false;
}

/**
 * \copydoc ListaCirc< T >::retiraDaPosicao( int posicao )
 */
template< typename T >
T ListaCirc< T >::retiraDaPosicao( int posicao )
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
            return retiraDoInicio( );
        } else
        {
            Elemento< T >* anterior = head->getProximo();
            
            for( int i = 0; i < posicao - 1; i++ )
            {
                anterior = anterior->getProximo( );
            }
            Elemento< T >* eliminar = anterior->getProximo( );
            
            //Variável auxiliar para o dado retornado
            T volta = eliminar->getInfo( );
            anterior->setProximo( eliminar->getProximo( ) );
            size--;
            delete eliminar;
            return volta;
        }
    }
}

/**
 * \copydoc ListaCirc< T >::adiciona( const T& dado )
 */
template< typename T >
void ListaCirc< T >::adiciona( const T& dado )
{
    return adicionaNaPosicao( dado, size );
}

/**
 * \copydoc ListaCirc< T >::retira()
 */
template< typename T >
T ListaCirc< T >::retira()
{
    return retiraDaPosicao( size - 1 );
}

/**
 * \copydoc ListaCirc< T >::retiraEspecifico( const T& dado )
 */
template< typename T >
T ListaCirc< T >::retiraEspecifico( const T& dado )
{
    if( listaVazia( ) )
    {
        throw ERROLISTAVAZIA; //ExcecaoListaVazia();
    }
    return retiraDaPosicao( posicao( dado ) );
}

/**
 * \copydoc ListaCirc< T >::adicionaEmOrdem( const T& dado )
 */
template< typename T >
void ListaCirc< T >::adicionaEmOrdem( const T& dado )
{
    if( listaVazia( ) )
    {
        return adicionaNoInicio( dado );
    }
    Elemento< T >* atual = head->getProximo();
    int posicao = 1;
    while( atual->getProximo( ) != 0 && maior( dado, atual->getInfo( ) ) )
    {
        atual = atual->getProximo( );
        posicao++;
    }
    if( maior( dado, atual->getInfo( ) ) )
    {
        return adicionaNaPosicao( dado, posicao + 1 );
    }
    adicionaNaPosicao( dado, posicao );
}

/**
 * \copydoc ListaCirc< T >::listaVazia() const
 */
template< typename T >
bool ListaCirc< T >::listaVazia() const
{
    if( size == 0 )
        return true;
    return false;
}

/**
 * \copydoc ListaCirc< T >::igual( T dado1, T dado2 )
 */
template< typename T >
bool ListaCirc< T >::igual( T dado1, T dado2 )
{
    return dado1 == dado2;
}

/**
 * \copydoc ListaCirc< T >::maior( T dado1, T dado2 )
 */
template< typename T >
bool ListaCirc< T >::maior( T dado1, T dado2 )
{
    return dado2 < dado1;
}

/**
 * \copydoc ListaCirc< T >::menor( T dado1, T dado2 )
 */
template< typename T >
bool ListaCirc< T >::menor( T dado1, T dado2 )
{
    return dado1 < dado2;
}

/**
 * \copydoc ListaCirc< T >::posicaoInvalida( int p )
 */
template< typename T >
bool ListaCirc< T >::posicaoInvalida( int p )
{
    return ( p >= size + 1 || p < 0 );
}

/* // ******************************************************************
 template< typename T >
 ListaCirc< T >* ListaCirc< T >::criaLista()
 {
 ListaCirc* aLista;
 aLista = new ListaCirc();
 if( aLista != NULL )
 {
 aLista->size = 0;
 aLista->head = NULL;
 }
 return aLista;
 }*/

/**
 * \copydoc ListaCirc< T >::destroiLista()
 */
template< typename T >
void ListaCirc< T >::destroiLista()
{
    while( size > 0 )
    {
        Elemento< T > * atual = head->getProximo();
        
        head->setProximo( atual->getProximo() );
        
        delete atual;
        
        this->size--;
    }
}

#endif /* LISTACIRC_HPP_ */
