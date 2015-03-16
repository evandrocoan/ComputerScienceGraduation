/*
 * Queue.h
 *
 */

#ifndef QUEUE_H_
#define QUEUE_H_

namespace BOOOS
{
    
    class Queue
    {
    public:
        Queue();
        virtual ~Queue();

        class Element
        {
        public:
            Element()
            {
                _prev = 0;
                _next = 0;
            }
            virtual ~Element()
            {
            }
            
            Element * prev()
            {
                return _prev;
            }
            Element * next()
            {
                return _next;
            }
            void prev( Element * p )
            {
                _prev = p;
            }
            void next( Element * n )
            {
                _next = n;
            }
            
        private:
            Element * _prev;
            Element * _next;
        };

        Element * head()
        {
            return &_head;
        }
        
        int length()
        {
            return _length;
        }
        
        void insert( Element * element )
        {
            if ( this->length() == 0 )
            {
                element->next( this->head() );
                this->head()->prev( element );
                
                element->prev( this->head() );
                this->head()->next( element );
                
                this->_length++;
            } else
            {
                // pega o prévio do elemento que o head aponta
                // faz o novo elemento apontar para o previo do head
                element->prev( this->head()->prev() );
                
                // fazer o novo elemeto apontar para o head
                element->next( this->head() );
            }
        }
        
        Element * remove()
        {
            if ( this->length() == 0 )
            {
                throw "A lista está vazia";
            }
            
            // pegar o proximo do head e devolver ele no return
            Element * element = this->head()->next();
            
            // fazer o head apontar para o segundo da lista
            this->head()->next( element->next() );
            
            return element;
        }
        
    private:
        Element _head;
        int _length;
    };

}

#endif /* QUEUE_H_ */ 
