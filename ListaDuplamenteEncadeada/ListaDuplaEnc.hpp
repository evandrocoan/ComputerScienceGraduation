/**
 * ListaEnc.hpp
 * 
 * \authors Evandro  Coan, Charles Borges de Lima
 */

/**
 * Evita incluir esta classe mais de uma vez no processo de compilação
 */
#ifndef LISTADUPLAENC_HPP_
#define LISTADUPLAENC_HPP_

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
class ListaDupla
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
        ListaDupla();

        /**
         * Destrutor padrão que destrói os ponteiros da lista e marca o 
         * tamanho da lista para zero
         */
        ~ListaDupla();

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
        T mostra(int pos);
        
        /**
         * Mostra qual o último elemento da lista duplamente encadeada.
         * 
         * @return o último elemento da lista encadeada
         */
        int verUltimo();
};

/**
 * \copydoc ListaDupla< T >::ListaEnc()
 */
template< typename T >
ListaDupla< T >::ListaDupla()
{
    head = NULL;
    size = 0;
}

/**
 * \copydoc ListaDupla< T >::~ListaEnc()
 */
template< typename T >
ListaDupla< T >::~ListaDupla()
{

}

/**
 * \copydoc ListaDupla< T >::adicionaNoInicioDuplo( const T& dado )
 */
template< typename T >
void ListaDupla< T >::adicionaNoInicioDuplo( const T& dado )
{
    ElementoDuplo< T > *novo = new ElementoDuplo< T >( dado, NULL, head );
    
    if( novo == NULL )
        throw ERROLISTADUPLACHEIA;
    else
    {
        //poderia ser feito diretamente em Elemento<T>(dado,head);
        //novo->setProximo(dados); 
        //novo->setProximo(head);  
        
        if( novo->getProximo() != NULL )
        {
            novo->getProximo()->setAnterior( novo );
        }
        head = novo;
        size++;
    }
}

/**
 * \copydoc ListaDupla< T >::retiraDoInicioDuplo()
 */
template< typename T >
T ListaDupla< T >::retiraDoInicioDuplo()
{
    if( listaVazia() )
    {
        throw ERROLISTADUPLAVAZIA;
    }
    ElementoDuplo< T >* saiu = head;
    T volta = saiu->getInfo();
    head = saiu->getProximo();
    size--;
    delete saiu;
    return volta;
}

/**
 * \copydoc ListaDupla< T >::eliminaDoInicioDuplo()
 */
template< typename T >
void ListaDupla< T >::eliminaDoInicioDuplo()
{
    if( listaVazia() )
    {
        throw ERROLISTADUPLAVAZIA;
    }
    ElementoDuplo< T >* saiu = head;
    head = saiu->getProximo();
    size--;
    delete saiu;
    return size;
}

/**
 * \copydoc ListaDupla< T >::tamanho()
 */
template< typename T >
int ListaDupla< T >::tamanho()
{
    return this->size;
}

/**
 * \copydoc ListaDupla< T >::adicionaNaPosicaoDuplo( const T& dado, int pos )
 */
template< typename T >
void ListaDupla< T >::adicionaNaPosicaoDuplo( const T& dado, int pos )
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
    
    // caso seja a última posição, chama o método que manipula a calda
//    if( pos == size )
//    {
//        return adiciona
//    }
    
    // aloca um elemento no inicio da lista
    ElementoDuplo< T >* novo = new ElementoDuplo< T >( dado, 0, 0 );
    
    // verifica se ela está cheia
    if( novo == 0 )
    {
        throw ERROLISTADUPLACHEIA;
    }
    
    // salva o ponterio da cabeça da lista
    ElementoDuplo< T >* anterior = head;
    
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
 * \copydoc ListaDupla< T >::posicaoDuplo( const T& dado ) const
 */
template< typename T >
int ListaDupla< T >::posicaoDuplo( const T& dado ) const
{
    if( listaVazia() )
    {
        throw -4; //ExcecaoListaVazia();
    }
    ElementoDuplo< T >* atual = head;
    for( int i = 0; i < size; i++ )
    {
        if( dado == atual->getInfo() )
        {
            return i;
        }
        atual = atual->getProximo();
    }
    throw -5; //ExcecaoDadoNaoEncontrado();
}

/**
 * \copydoc ListaDupla< T >::posicaoMemDuplo( const T& dado ) const
 */
template< typename T >
T* ListaDupla< T >::posicaoMemDuplo( const T& dado ) const
{
    if( listaVazia() )
    {
        throw -1; //ExcecaoListaVazia();
    }
    int posicao = posicaoDuplo( dado );
    ElementoDuplo< T >* atual = head;
    for( int i = 0; i < posicao; i++ )
    {
        atual = atual->getProximo();
    }
    return atual->getInfo();
}

/**
 * \copydoc ListaDupla< T >::contemDuplo( const T& dado )
 */
template< typename T >
bool ListaDupla< T >::contemDuplo( const T& dado )
{
    if( listaVazia() )
    {
        throw -2; //ExcecaoListaVazia();
    }
    ElementoDuplo< T >* atual = head;
    for( int i = 0; i < size; i++ )
    {
        if( igual( dado, atual->getInfo() ) )
        {
            return true;
        }
        atual = atual->getProximo();
    }
    return false;
}

/**
 * \copydoc ListaDupla< T >::retiraDaPosicaoDuplo( int posicao )
 */
template< typename T >
T ListaDupla< T >::retiraDaPosicaoDuplo( int posicao )
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
            return retiraDoInicioDuplo();
        } else
        {
            ElementoDuplo< T >* anterior = head;
            
            for( int i = 0; i < posicao - 1; i++ )
            {
                anterior = anterior->getProximo();
            }
            ElementoDuplo< T >* eliminar = anterior->getProximo();
            
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
 * \copydoc ListaDupla< T >::adicionaDuplo( const T& dado )
 */
template< typename T >
void ListaDupla< T >::adicionaDuplo( const T& dado )
{
    return adicionaNaPosicaoDuplo( dado, size );
}

/**
 * \copydoc ListaDupla< T >::retiraDuplo()
 */
template< typename T >
T ListaDupla< T >::retiraDuplo()
{
    return retiraDaPosicaoDuplo( size - 1 );
}

/**
 * \copydoc ListaDupla< T >::retiraEspecificoDuplo( const T& dado )
 */
template< typename T >
T ListaDupla< T >::retiraEspecificoDuplo( const T& dado )
{
    if( listaVazia() )
    {
        throw ERROLISTADUPLAVAZIA; //ExcecaoListaVazia();
    }
    return retiraDaPosicaoDuplo( posicaoDuplo( dado ) );
}

/**
 * \copydoc ListaDupla< T >::adicionaEmOrdem( const T& dado )
 */
template< typename T >
void ListaDupla< T >::adicionaEmOrdem( const T& dado )
{
    if( listaVazia() )
    {
        return adicionaNoInicioDuplo( dado );
    }
    ElementoDuplo< T >* atual = head;
    int posicao = 1;
    while( atual->getProximo() != 0 && maior( dado, atual->getInfo() ) )
    {
        atual = atual->getProximo();
        posicao++;
    }
    if( maior( dado, atual->getInfo() ) )
    {
        return adicionaNaPosicaoDuplo( dado, posicao + 1 );
    }
    adicionaNaPosicaoDuplo( dado, posicao );
}

/**
 * \copydoc ListaDupla< T >::listaVazia() const
 */
template< typename T >
bool ListaDupla< T >::listaVazia() const
{
    if( size == 0 )
        return true;
    return false;
}

/**
 * \copydoc ListaDupla< T >::igual( T dado1, T dado2 )
 */
template< typename T >
bool ListaDupla< T >::igual( T dado1, T dado2 )
{
    return dado1 == dado2;
}

/**
 * \copydoc ListaDupla< T >::maior( T dado1, T dado2 )
 */
template< typename T >
bool ListaDupla< T >::maior( T dado1, T dado2 )
{
    return dado2 < dado1;
}

/**
 * \copydoc ListaDupla< T >::menor( T dado1, T dado2 )
 */
template< typename T >
bool ListaDupla< T >::menor( T dado1, T dado2 )
{
    return dado1 < dado2;
}

/**
 * \copydoc ListaDupla< T >::posicaoInvalida( int p )
 */
template< typename T >
bool ListaDupla< T >::posicaoInvalida( int p )
{
    return ( p >= size + 1 || p < 0 );
}

/* // ******************************************************************
template< typename T >
ListaEnc< T >* ListaDupla< T >::criaLista()
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
 * \copydoc ListaDupla< T >::destroiListaDuplo()
 */
template< typename T >
void ListaDupla< T >::destroiListaDuplo()
{
    while( head )
    {
        ElementoDuplo< T > * atual = head;
        
        head = head->getProximo();
        
        delete atual;
        
        this->size--;
        
        // evitar acessar um null pointer
        if(atual != 0)
        {
           atual = atual->getProximo();
        }
    }
}

/**
 * \copydoc T ListaDupla< T >::mostra(int pos)
 */
template< typename T >
T ListaDupla< T >::mostra(int pos)
{
    T elemento;
    
    elemento = this->retiraDaPosicaoDuplo( pos );
    
    this->adicionaNaPosicaoDuplo( elemento, pos );
    
    return elemento;
}

/**
 * \copydoc ListaDupla< T >::verUltimo();
 */
template< typename T >
int ListaDupla< T >::verUltimo()
{
    return this->tamanho();
}

#endif /* LISTADUPLAENC_HPP_ */
