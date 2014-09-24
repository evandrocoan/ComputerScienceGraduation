/*
 * Elemento.hpp
 *
 *  Created on: 13/09/2014
 *      Author: Carcara
 */

#ifndef ELEMENTO_HPP_
#define ELEMENTO_HPP_

template< typename T >
class ElementoDuplo
{
    private:
        /**
         * Ponteiro para o tipo de informação que vai ser armazenada
         */
        T *info;

        /**
         * Ponteiro para o próximo elemento da lista
         */
        ElementoDuplo< T >* _next;

    public:
        Elemento( const T& info, ElementoDuplo< T >* next ) :
                info( new T( info ) ), _next( next )
        {
        }
        
        //-----------------------------------------------------------------
        ~ElementoDuplo()
        {
            delete info;
        }
        //-----------------------------------------------------------------
        ElementoDuplo< T >* getProximo() const
        {
            return _next;
        }
        //-----------------------------------------------------------------
        T getInfo() const
        {
            return *info;
        }
        //-----------------------------------------------------------------
        void setProximo( ElementoDuplo< T >* next )
        {
            _next = next;
        }
        //-----------------------------------------------------------------
};

#endif /* ELEMENTO_HPP_ */
