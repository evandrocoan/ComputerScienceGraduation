//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/**
 * ListaEnc.hpp
 * 
 * \authors Evandro  Coan, Charles Borges de Lima
 */

/**
 * Evita incluir esta classe mais de uma vez no processo de compilação
 */
#ifndef LISTACIRCDUPLA_HPP_
#define LISTACIRCDUPLA_HPP_

/**
 * Bibliotecas utilizadas
 */
#include <stdio.h>
#include <stdlib.h>
#include "ElementoDuplo.hpp"

/**
 * Códigos de erros definidos
 */
#define ERROLISTADUPLACHEIA -1
#define ERROLISTADUPLAVAZIA -2
#define ERRODEPOSICAO -3

/**
 * Header para um lista duplamente encadeada
 */
template< typename T >
class ListaDuplaCirc
{
    
private:
    /**
     * Ponteiro para o primeiro elemento da lista
     */
    ElementoDuplo< T >* head;

    /**
     * Número de elementos da lista
     */
    int size;

public:
    
    /**
     * Construtor padrão que cria uma lista duplamente encadeada vazia
     */
    ListaDuplaCirc();

    /**
     * Destrutor padrão que destrói os ponteiros da lista e marca o 
     * tamanho da lista para zero
     */
    ~ListaDuplaCirc();

    /**
     * Informa o tamanho atual da lista duplamente encadeada. 
     * 
     * @return um inteiro informando o tamanho da lista encadeada
     */
    int tamanho();

    /*
     // ****************************
     ListaEnc< T >* criaLista();
     // ****************************
     */
    /**
     * Adiciona um dado recebido como parâmetro no início da lista
     * 
     * @param dado
     */
    void adicionaNoInicioDuplo( const T& dado );

    /**
     * Retira o elemento da cabeça da lista encadeada
     * 
     * @return o elemento na cabeça da lista encadeada
     */
    T retiraDoInicioDuplo();

    /**
     * Deleta o elemento no ínicio na cabeça da lista encadeada
     * 
     */
    void eliminaDoInicioDuplo();

    /**
     * Adiciona um elemento em uma posição específica.
     * 
     * @param dado a ser adicionado
     * @param pos a posição para adicionar o dado
     * @throw erro de posição se a posição é inexistente
     */
    void adicionaNaPosicaoDuplo( const T& dado, int pos );

    /**
     * Retorna a posição de um elemento fornecido como parâmetro. 
     * A posição nesta lista encadeada começa em 0, para o primeiro 
     * elemento.
     * 
     * @param dado a ter a posição encontrada
     * @return a posição do dado encontrado com um inteiro
     */
    int posicaoDuplo( const T& dado ) const;

    /**
     * Descobre qual o ponteiro para um determinado elemento na lista.
     * 
     * @param dado a ser procurado um ponteiro
     * @return o ponteiro para o dado passado como parâmetro
     */
    T* posicaoMemDuplo( const T& dado ) const;

    /**
     * Informa se a lista encadeada contém o dado passado como parâmetro.
     * 
     * @param dado a ser encontrado
     * @return true se a lista encadeada contém o dada passado como 
     * parâmetro, false caso contrário.
     */
    bool contemDuplo( const T& dado );

    /**
     * Elimina um elemento de uma certa posição nesta lista encadeada.
     * 
     * @param a posição do elemento a ser retirado
     * @return o elemento retirado da posição
     */
    T retiraDaPosicaoDuplo( int pos );

    /**
     * Adiciona um elemento no final da lista encadeada
     * 
     * @param dado a ser adicionado
     */
    void adicionaDuplo( const T& dado );

    /**
     * Retira o elemento pertencente ao final da lista encadeada
     * 
     * @return o dado retirado da lista encadeada
     */
    T retiraDuplo();

    /**
     * Retira a primeiro ocorrência do dado fornecidado como parâmetro
     * 
     * @param dado a ter a primeira ocorrência retirada
     * @return a primeira ocorrência do dado retirado
     */
    T retiraEspecificoDuplo( const T& dado );

    /**
     * Adiciona um dado a lista encadeada, na ordem definida pelo tipo
     * de objeto que essa lista encadeada reprensenta
     * 
     * @param dado a ser adicionada em ordem
     */
    void adicionaEmOrdem( const T& data );

    /**
     * Informa se a lista encadeada está vazia
     * 
     * @return true se a lista encadeada está vazia, false caso contrário
     */
    bool listaVazia() const;

    /**
     * Informa se dois dados são iguais. Utiliza o operador de igualdade 
     * do tipo do objeto que essa lista encadeada representa para 
     * determinar se eles são iguais.
     * 
     * @param dado1 a ser comparado
     * @param dado2 a ser comparado
     * @return true se os parametrôs são iguais, false caso contrário
     */
    bool igual( T dado1, T dado2 );

    /**
     * Informa se o primeiro parâmetro é maior que o segundo. Utiliza o 
     * operador de igualdade do tipo do objeto que essa lista encadeada 
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
     * operador de igualdade do tipo do objeto que essa lista encadeada 
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
    void destroiListaDuplo();

    /**
     * Informa se uma determinada posição válida.
     * 
     * @param a posição a ser verificada
     * @return true se a posição é válida, false caso contrário
     */
    bool posicaoInvalida( int p );

    /**
     * Mostra qual elemento está em uma posição específica.
     * 
     * @param posição do elemento a ser visto
     * @return o elemento encontrado na posição requerida
     */
    T mostra( int pos );

    /**
     * Mostra qual o último elemento da lista duplamente encadeada.
     * 
     * @return o último elemento da lista encadeada
     */
    int verUltimo();
};

/**
 * \copydoc ListaDuplaCirc< T >::ListaEnc()
 */
template< typename T >
ListaDuplaCirc< T >::ListaDuplaCirc()
{
    // cria-se o nodo sentinela
    head = new ElementoDuplo< T >( NULL, NULL , NULL);
    size = 0;
}

/**
 * \copydoc ListaDuplaCirc< T >::~ListaEnc()
 */
template< typename T >
ListaDuplaCirc< T >::~ListaDuplaCirc()
{
    this->destroiListaDuplo( );
}

/**
 * \copydoc ListaDuplaCirc< T >::adicionaNoInicioDuplo( const T& dado )
 */
template< typename T >
void ListaDuplaCirc< T >::adicionaNoInicioDuplo( const T& dado )
{
    // quando se adiciona pela primeira vez
    if( this->size == 0 )
    {
        ElementoDuplo< T > *novo = new ElementoDuplo< T >( dado, head, head );
        
        head->setProximo( novo );
        head->setAnterior( novo );
        
        size++;
    } else
    {
        // salva o segundo (futuro terceiro) dado da lista
        // para emendar com novo segundo dado
        ElementoDuplo< T >* segundoDado = head->getProximo( );
        
        // aloca um elemento no inicio da lista
        ElementoDuplo< T >* novoSegundoDado = new ElementoDuplo< T >( dado, segundoDado, head );
        
        // verifica se ela está cheia
        if( novoSegundoDado == 0 )
        {
            throw ERROLISTADUPLACHEIA;
        }
        
        // agora faz o head apontar para o novo segundo dado
        head->setProximo( novoSegundoDado );
        
        size++;
    }
}

/**
 * \copydoc ListaDuplaCirc< T >::retiraDoInicioDuplo()
 */
template< typename T >
T ListaDuplaCirc< T >::retiraDoInicioDuplo()
{
    if( listaVazia( ) )
    {
        throw ERROLISTADUPLACHEIA;
    }
    ElementoDuplo< T >* saiu = head->getProximo();
    T volta = saiu->getInfo( );
    head->setProximo( saiu->getProximo( ) );
    size--;
    delete saiu;
    return volta;
}

/**
 * \copydoc ListaDuplaCirc< T >::eliminaDoInicioDuplo()
 */
template< typename T >
void ListaDuplaCirc< T >::eliminaDoInicioDuplo()
{
    if( listaVazia( ) )
    {
        throw ERROLISTADUPLACHEIA;
    }
    ElementoDuplo< T >* saiu = head->getProximo();
    T volta = saiu->getInfo( );
    head->setProximo( saiu->getProximo( ) );
    size--;
    delete saiu;
}

/**
 * \copydoc ListaDuplaCirc< T >::tamanho()
 */
template< typename T >
int ListaDuplaCirc< T >::tamanho()
{
    return this->size;
}

/**
 * \copydoc ListaDuplaCirc< T >::adicionaNaPosicaoDuplo( const T& dado, int pos )
 */
template< typename T >
void ListaDuplaCirc< T >::adicionaNaPosicaoDuplo( const T& dado, int pos )
{
    // verifica se está tentando adicionar em uma posição inexistente
    if( posicaoInvalida( pos ) )
    {
        throw ERRODEPOSICAO;
    }
    
    // caso seja a primeia posição, chama o método que manipula a cabeça
    if( pos == 0 )
    {
        return adicionaNoInicioDuplo( dado );
    }
    
    // aloca um elemento no inicio da lista
    ElementoDuplo< T >* novo = new ElementoDuplo< T >( dado, 0, 0 );
    
    // verifica se ela está cheia
    if( novo == 0 )
    {
        throw ERROLISTADUPLACHEIA;
    }
    
    // salva o ponterio da cabeça da lista
    ElementoDuplo< T >* anterior = head->getProximo();
    
    // faz a troca dos elementos
    for( int i = 0; i < pos - 1; i++ )
    {
        anterior = anterior->getProximo( );
    }
    
    // emenda a cabeça na lista (anterior) no novo elemento
    novo->setProximo( anterior->getProximo( ) );
    
    //Se o novo não é o último da lista…
    if( novo->getProximo( ) != NULL )
    {
        //Faço o antecessor do sucessor do novo.
        novo->getProximo( )->setAnterior( novo );
    }
    
    anterior->setProximo( novo );
    
    novo->setAnterior( anterior );
    
    size++;
}

/**
 * \copydoc ListaDuplaCirc< T >::posicaoDuplo( const T& dado ) const
 */
template< typename T >
int ListaDuplaCirc< T >::posicaoDuplo( const T& dado ) const
{
    if( listaVazia( ) )
    {
        throw -4; //ExcecaoListaVazia();
    }
    ElementoDuplo< T >* atual = head->getProximo();
    for( int i = 0; i < size; i++ )
    {
        if( dado == atual->getInfo( ) )
        {
            return i;
        }
        atual = atual->getProximo( );
    }
    throw -5; //ExcecaoDadoNaoEncontrado();
}

/**
 * \copydoc ListaDuplaCirc< T >::posicaoMemDuplo( const T& dado ) const
 */
template< typename T >
T* ListaDuplaCirc< T >::posicaoMemDuplo( const T& dado ) const
{
    if( listaVazia( ) )
    {
        throw -1; //ExcecaoListaVazia();
    }
    int posicao = posicaoDuplo( dado );
    ElementoDuplo< T >* atual = head->getProximo();
    for( int i = 0; i < posicao; i++ )
    {
        atual = atual->getProximo( );
    }
    return atual->getInfo( );
}

/**
 * \copydoc ListaDuplaCirc< T >::contemDuplo( const T& dado )
 */
template< typename T >
bool ListaDuplaCirc< T >::contemDuplo( const T& dado )
{
    if( listaVazia( ) )
    {
        throw -2; //ExcecaoListaVazia();
    }
    ElementoDuplo< T >* atual = head->getProximo();
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
 * \copydoc ListaDuplaCirc< T >::retiraDaPosicaoDuplo( int posicao )
 */
template< typename T >
T ListaDuplaCirc< T >::retiraDaPosicaoDuplo( int posicao )
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
            return retiraDoInicioDuplo( );
        } else
        {
            ElementoDuplo< T >* anterior = head->getProximo();
            
            for( int i = 0; i < posicao - 1; i++ )
            {
                anterior = anterior->getProximo( );
            }
            ElementoDuplo< T >* eliminar = anterior->getProximo( );
            
            //Variável auxiliar para o dado retornado
            T volta = eliminar->getInfo( );
            
            anterior->setProximo( eliminar->getProximo( ) );
            
            if( eliminar->getProximo( ) != NULL )
            {
                eliminar->getProximo( )->setAnterior( anterior );
            }
            
            size--;
            delete eliminar;
            return volta;
        }
    }
}

/**
 * \copydoc ListaDuplaCirc< T >::adicionaDuplo( const T& dado )
 */
template< typename T >
void ListaDuplaCirc< T >::adicionaDuplo( const T& dado )
{
    return adicionaNaPosicaoDuplo( dado, size );
}

/**
 * \copydoc ListaDuplaCirc< T >::retiraDuplo()
 */
template< typename T >
T ListaDuplaCirc< T >::retiraDuplo()
{
    return retiraDaPosicaoDuplo( size - 1 );
}

/**
 * \copydoc ListaDuplaCirc< T >::retiraEspecificoDuplo( const T& dado )
 */
template< typename T >
T ListaDuplaCirc< T >::retiraEspecificoDuplo( const T& dado )
{
    if( listaVazia( ) )
    {
        throw ERROLISTADUPLAVAZIA; //ExcecaoListaVazia();
    }
    return retiraDaPosicaoDuplo( posicaoDuplo( dado ) );
}

/**
 * \copydoc ListaDuplaCirc< T >::adicionaEmOrdem( const T& dado )
 */
template< typename T >
void ListaDuplaCirc< T >::adicionaEmOrdem( const T& dado )
{
    if( listaVazia( ) )
    {
        return adicionaNoInicioDuplo( dado );
    }
    
    ElementoDuplo< T >* atual = head->getProximo();
    int posicao = 1;
    
    while( atual->getProximo( ) != NULL && maior( dado, atual->getInfo( ) ) )
    {
        //Encontrar posição para inserir.
        atual = atual->getProximo( );
        posicao++;
    }
    
    if( maior( dado, atual->getInfo( ) ) )
    {
        return adicionaNaPosicaoDuplo( dado, posicao + 1 );
    }
    
    adicionaNaPosicaoDuplo( dado, posicao );
}

/**
 * \copydoc ListaDuplaCirc< T >::listaVazia() const
 */
template< typename T >
bool ListaDuplaCirc< T >::listaVazia() const
{
    if( size == 0 )
        return true;
    return false;
}

/**
 * \copydoc ListaDuplaCirc< T >::igual( T dado1, T dado2 )
 */
template< typename T >
bool ListaDuplaCirc< T >::igual( T dado1, T dado2 )
{
    return dado1 == dado2;
}

/**
 * \copydoc ListaDuplaCirc< T >::maior( T dado1, T dado2 )
 */
template< typename T >
bool ListaDuplaCirc< T >::maior( T dado1, T dado2 )
{
    return dado2 < dado1;
}

/**
 * \copydoc ListaDuplaCirc< T >::menor( T dado1, T dado2 )
 */
template< typename T >
bool ListaDuplaCirc< T >::menor( T dado1, T dado2 )
{
    return dado1 < dado2;
}

/**
 * \copydoc ListaDuplaCirc< T >::posicaoInvalida( int p )
 */
template< typename T >
bool ListaDuplaCirc< T >::posicaoInvalida( int p )
{
    return ( p >= size + 1 || p < 0 );
}

/* // ******************************************************************
 template< typename T >
 ListaEnc< T >* ListaDuplaCirc< T >::criaLista()
 {
 ListaEnc* aLista;
 aLista = new ListaEnc();
 if( aLista != NULL )
 {
 aLista->size = 0;
 aLista->head = NULL;
 }
 return aLista;
 }*/

/**
 * \copydoc ListaDuplaCirc< T >::destroiListaDuplo()
 */
template< typename T >
void ListaDuplaCirc< T >::destroiListaDuplo()
{
    while( size > 0 )
    {
        ElementoDuplo< T > * atual = head->getProximo();
        
        head->setProximo( atual->getProximo() );
        
        delete atual;
        
        this->size--;
    }
}

/**
 * \copydoc T ListaDuplaCirc< T >::mostra(int pos)
 */
template< typename T >
T ListaDuplaCirc< T >::mostra( int pos )
{
    T elemento;
    
    elemento = this->retiraDaPosicaoDuplo( pos );
    
    this->adicionaNaPosicaoDuplo( elemento, pos );
    
    return elemento;
}

/**
 * \copydoc ListaDuplaCirc< T >::verUltimo();
 */
template< typename T >
int ListaDuplaCirc< T >::verUltimo()
{
    return this->tamanho( );
}

#endif /* LISTACIRCDUPLA_HPP_ */
