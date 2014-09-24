/*
 * Elemento.hpp
 *
 *  Created on: 13/09/2014
 *      Author: Carcara
 */

#ifndef ELEMENTO_HPP_
#define ELEMENTO_HPP_

template< typename T >
class Elemento
{
    private:
        /**
         * Ponteiro para o tipo de informação que vai ser armazenada
         */
        T *info;

        /**
         * Ponteiro para o próximo elemento da lista
         */
        Elemento< T >* _next;

        /**
         * Ponteiro para o elemento elemento anterior da lista
         */
        Elemento< T >* _previous;

    public:
        Elemento( const T& info, Elemento< T >* next, 
                  Elemento< T >* previous ) :
                info( new T( info ) ), _next( next ), _previous( previous )
        {
        }
        
        /**
         * Destrói o ponteiro para o elemento que esse nó ponta
         */
        ~Elemento()
        {
            delete info;
        }
        
        /**
         *  Retorna um ponterio para o próximo elemento deste nó.
         * 
         * @return um ponteiro para o próximo elemento, NULL caso não exista.
         */
        Elemento< T >* getProximo() const
        {
            return _next;
        }
        
        /**
         *  Retorna um ponterio para o elemento anterior deste nó.
         * 
         * @return um ponteiro para o elemento anterior deste nó, NULL caso 
         * não exista.
         */
        Elemento< T >* getAnterior() const
        {
            return _previous;
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
        void setProximo( Elemento< T >* next )
        {
            _next = next;
        }
        
        /**
         * Altera qual o elemento anterior que este nó aponta
         * 
         * @param um ponteiro para nó anterior
         */
        void setAnterior( Elemento< T >* previous )
        {
            _previous = previous;
        }
        //-----------------------------------------------------------------
};

#endif /* ELEMENTO_HPP_ */
