//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/**
 * Arquivo que contém a implementação de uma árvore binária que utiliza 
 * templates.
 *
 * \authors Evandro  Coan, Charles Borges de Lima
 */

#ifndef NODEARVORE_HPP_
#define NODEARVORE_HPP_

/**
 * Classe que representa um nó de uma árvore binária
 */
template< typename T >
class NodeArvore
{
private:
    /**
     * Ponteiro para o tipo de informação que vai ser armazenada
     */
    T *info;

    /**
     * Ponteiro para o próximo elemento da árvore
     */
    NodeArvore< T >* _right;

    /**
     * Ponteiro para o elemento elemento anterior da árvore
     */
    NodeArvore< T >* _left;

    /**
     *  Inteiro que armazena a altura (nível do nodo)
     */
    int height;

public:
    NodeArvore( const T& info, NodeArvore< T >* next,
                NodeArvore< T >* previous, int height ) :
            info( new T( info ) ), _right( next ), _left( previous ), 
            height( height )
    {
    }
    
    /**
     * Destrói o ponteiro para o elemento que esse nó ponta
     */
    ~NodeArvore()
    {
        delete info;
    }
    
    /**
     *  Retorna um ponterio para o próximo elemento deste nó.
     * 
     * @return um ponteiro para o próximo elemento, NULL caso não exista.
     */
    NodeArvore< T >* getProximo() const
    {
        return _right;
    }
    
    /**
     *  Retorna um ponterio para o elemento anterior deste nó.
     * 
     * @return um ponteiro para o elemento anterior deste nó, NULL caso 
     * não exista.
     */
    NodeArvore< T >* getAnterior() const
    {
        return _left;
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
    void setProximo( NodeArvore< T >* next )
    {
        _right = next;
    }
    
    /**
     * Altera qual o elemento anterior que este nó aponta
     * 
     * @param um ponteiro para nó anterior
     */
    void setAnterior( NodeArvore< T >* previous )
    {
        _left = previous;
    }
    
    /**
     * Retorna a altura no nodo com um inteiro
     * 
     * @return um inteiro representando a altura do nó
     */
    int getHeight() const
    {
        return height;
    }
    
    /**
     * Ajusta a altura do nó.
     * 
     * @param um inteiro informando qual a altura do nó
     */
    void setHeight( int height )
    {
        this->height = height;
    }
};

#endif /* NODEARVORE_HPP_ */
