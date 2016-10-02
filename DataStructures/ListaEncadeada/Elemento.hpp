//! Copyright year [2014] <Evandro  Coan, Charles Borges de Lima>
/**
 * ListaEnc.hpp
 * 
 * \authors Evandro  Coan, Charles Borges de Lima
 */

#ifndef ELEMENTO_HPP_
#define ELEMENTO_HPP_

/**
 * Classe que representa um nó de uma lista encadeada
 */
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

    public:
        Elemento( const T& info, Elemento< T >* next ) :
                info( new T( info ) ), _next( next )
        {
        }
        
        //-----------------------------------------------------------------
        ~Elemento()
        {
            delete info;
        }
        //-----------------------------------------------------------------
        Elemento< T >* getProximo() const
        {
            return _next;
        }
        //-----------------------------------------------------------------
        T getInfo() const
        {
            return *info;
        }
        //-----------------------------------------------------------------
        void setProximo( Elemento< T >* next )
        {
            _next = next;
        }
        //-----------------------------------------------------------------
};

#endif /* ELEMENTO_HPP_ */
