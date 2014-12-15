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
    virtual ~NoBinario()
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
    NoBinario< T >* inserir( const T& info, NoBinario< T >* raiz )
    {
        //caso o dado seja seja menor do que a raiz atual
        if( info < * ( raiz->getDado( ) ) )
        {
            //faz a inserção a esquerda
            if( raiz->getEsquerda( ) == NULL )
            {
                /* oNovo < -aloque( tNodo );
                 * oNovo->info < -info;
                 * oNovo->filhoÀEsquerda < -NULO;
                 * oNovo->filhoÀDireita < -NULO;
                 * raiz->filhoÀEsquerda < -oNovo
                 */
                NoBinario< T >* oNovo = new NoBinario< T >( info );
                raiz->setEsquerda( oNovo );
                return raiz;
            } else
            {
                /*raiz <- inserção(raiz->filhoÀEsquerda, info);
                 */
                raiz->inserir( info, raiz->getEsquerda( ) );
                return raiz;
            }
        } else
        {
            /*// Inserção à direita.
             * se (raiz->filhoÀDireita = NULO) então
             * oNovo <- aloque(tNodo);
             * oNovo->info <- info;
             * oNovo->filhoÀEsquerda <- NULO;
             * raiz->filhoÀDireita <- oNovo;
             */
            if( raiz->getDireita( ) == NULL )
            {
                NoBinario< T >* oNovo = new NoBinario< T >( info );
                raiz->setDireita( oNovo );
                return raiz;
            } else
            {
                /* senão
                 * raiz <- inserção(raiz->filhoÀDireita, info);
                 */
                raiz->inserir( info, raiz->getDireita( ) );
                return raiz;
            }
        }
    }
    
    /**
     * Remove um dado fornecido da árvore fornecida e retorna a árvore 
     * atualizada com a remoção
     * 
     * @param árvore a ter o dado removido
     * @param dado a ser removido da árvore
     * @return
     */
    NoBinario< T >* remover( NoBinario< T >* arv, const T& info )
    {
        /*se (arv = NULO) então
         retorne arv*/
        if( arv->getDado( ) == NULL )
        {
            return arv;
        } else
        {
            /*senão
             se (info < arv->info) // Vá à esquerda.*/
            if( info < * ( arv->getDado( ) ) )
            {
                /*arv->filhoÀEsquerda <- delete(info, arv->filhoÀEsquerda);
                 retorne arv;*/
                arv->setEsquerda( delete ( info, arv->getEsquerda( ) ) );
                return arv;
            } else
            {
                /*senão
                 se (info > arv->info) // Vá à direita.*/
                if( info > arv->getDado( ) )
                {
                    /*arv->filhoÀDireita <- delete(info, arv->filhoÀDireita);
                     retorne arv;*/
                    arv->setDireita( delete ( info, arv->getDireita( ) ) );
                    return arv;
                } else
                {
                    /*senão // Encontrei elemento que quero deletar.*/
                    // 2 filhos.
                    /*se (arv->filhoÀDireita ~= NULO E arv->filhoÀEsquerda ~= NULO)
                     tmp <- mínimo(arv->filhoÀDireita);
                     arv->info <- tmp->info;
                     arv->filhoÀDireita <- delete(arv->info, arv->filhoÀDireita);
                     retorne arv;*/
                    if( arv->getDireita( ) != NULL && arv->getEsquerda( )
                            != NULL )
                    {
                        NoBinario< T > tmp = this->minimo( arv );
                        arv->setDado( tmp.getDado( ) );
                        arv->setDireita(
                                delete ( arv->getDado( ), arv->getDireita( ) ) );
                        return arv;
                    } else
                    {
                        /*senão // 1 filho.
                         tmp <- arv;*/
                        NoBinario< T > tmp = arv;
                        
                        /*se (arv->filhoÀDireita ~= NULO) então 
                         * // Filho à direita.
                         filho <- arv->filhoÀDireita;
                         retorne filho;*/
                        if( arv->getDireita( ) != NULL )
                        {
                            NoBinario< T > filho = arv->getDireita( );
                            return filho;
                        } else
                        {
                            /*senão
                             * // Filho à esquerda.
                             se (arv->filhoÀEsquerda ~= NULO) então 
                             filho <- arv->filhoÀEsquerda;
                             retorne filho;*/
                            if( arv->getEsquerda( ) != NULL )
                            {
                                NoBinario< T > filho = arv->getEsquerda( );
                                return filho;
                            } else
                            {
                                /*senão // Folha.
                                 libere arv;
                                 retorne NULO;*/
                                delete ( arv );
                                return NULL;
                            }
                        }
                    }
                }
            }
        }
    }
    
    /**
     * Retorna o menor nó de uma árvore.
     * 
     * @param um ponteiro para a arvore a ter o menor nó encontrado
     * @return um ponteiro para o menor nó encontrado
     */
    NoBinario< T >* minimo( NoBinario< T >* nodo )
    {
        if( nodo->getDireita( ) < nodo->getEsquerda( ) )
        {
            return nodo->getDireita( );
        }
        return nodo->getEsquerda( );
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
    T* getDado() const
    {
        return this->dado;
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
    
    /**
     * Busca um elemento fornecido na árvore fornecida.
     * 
     * @param dado a ser encontrado na árvore fornecida
     * @param arvore fornecida para ter o dado procurado
     * @return o dado procurado na arvore fornecida, NULL caso não seja 
     * encontrado
     */
    T* busca( const T& chave, NoBinario< T >* ptr )
    {
        /*enquanto (ptr ~= NULO
         E ptr->info ~= chave) faça
         // Esquerda ou direita*/
        while( ptr != NULL && ptr->getDado( ) != chave )
        {
            /*se (ptr->info < chave) então
             ptr <- ptr->filhoÀDireita*/
            if( ptr->getDado( ) < chave )
            {
                ptr = ptr->getDireita( );
            } else
            {
                /*senão
                 ptr <- ptr->filhoÀEsquerda;*/
                ptr = ptr->getEsquerda( );
            }
        }
        return ptr;
    }
    
    NoBinario< T >* getElementos()
    {
        return this->elementos;
    }
    
    void preOrdem( NoBinario< T >* raiz )
    {
        /*se raiz != NULO então
         imprime(raiz->info);
         Preordem(raiz->filhoEsquerda);
         Preordem(raiz->filhoDireita);
         fim se*/
        if( raiz != NULL )
        {
            this->elementos.push_back( * ( raiz->getDado( ) ) );
            this->preOrdem( raiz->getDireita( ) );
            this->preOrdem( raiz->getEsquerda( ) );
        }
    }
    
    void emOrdem( NoBinario< T >* nodo )
    {
    }
    
    void posOrdem( NoBinario< T >* nodo )
    {
    }
};

#endif /* NOBINARIO_HPP_ */
