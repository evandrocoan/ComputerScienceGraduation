/*
 * BOOOS.h
 *
 *  Created on: Aug 14, 2014
 */

#ifndef QUEUE_H_
#define QUEUE_H_


/**
* @see BOOOS namespace declaration at the main file 'BOOOS.h'.
*/
namespace BOOOS
{
    /**
     * Implements a queue abtraction using a double linked list.
     */
    class Queue
    {
    public:
        
        /**
         * To create an empty Queue object ready to receive elements.
         */
        Queue();
        
        /**
         * Free the memory used by this object.
         */
        virtual ~Queue();
        
        
        /**
         * Implements a queue node which holds its information as its rank, next and previous
         * elements on the queue. This elementa data must to be explicitly updated every time
         * the queue is changed.
         */
        class Element
        {
        public:
            
            /**
             * Creates a new Element object.
             */
            Element() { this->_prev = 0; this->_next = 0; this->_rank = 0; }
            
            /**
             * Free the memory used by this object.
             */
            virtual ~Element() { delete this->_prev; delete this->_next; delete this->_rank; }
            
            /**
             * Gets the previus element this double linked list is pointing to.
             * 
             * @return null when there is not any next element, otherwise, an Element object
             *         pointer.
             */
            Element* prev() { return this->_prev; }
            
            /**
             * Gets the next element this double linked list is pointing to.
             * 
             * @return null when there is not any next element, otherwise, an Element object
             *      pointer.
             */
            Element* next() { return this->_next; }
            
            /**
             * Gets this element rank on the double linked list.
             * 
             * @return this element rank.
             */
            int rank() { return this->_rank; }
            
            /**
             * Sets the previous element to this on the double linked list.
             * 
             * @param p    the previous element to this on the linked list.
             */
            void prev( Element* p ) { this->_prev = p; }
            
            /**
             * Sets the next element to this on the double linked list.
             * 
             * @param p    the next element to this on the double linked list.
             */
            void next( Element* p ) { this->_next = p; }
            
            /**
             * Set this element rank on the linked list.
             * 
             * @param r    this element rank on the double linked list.
             */
            void rank(int r) { this->_rank = r; }
            
            
        private:
            
            /**
             * Holds the previous element pointer to this current element on the double linked
             * list.
             */
            Element* _prev;
            
            /**
             * Holds the next element pointer to this current element on the double linked list.
             */
            Element* _next;
            
            /**
             * Holds current element rank on the double linked list.
             */
            int _rank;
        };
        
        
        /**
         * Gets the current double linked list head element. Calling 'head()->next()' will point
         * to its head, and calling 'head()->prev()' will point to tail.
         * 
         * @return an Element pointer to the current double linked list head.
         */
        Element* head() { return &( this->_head ); }
        
        /**
         * 
         */
        int length() { return this->_length; }
        
        /**
         * 
         */
        void insert( Element* elem );
        
        /**
         * 
         */
        void insert_ordered( Element* elem );
        
        /**
         * 
         */
        Element* remove();
        
        /**
         * 
         */
        void remove( Element* e );
        
        
    private:
        
        /**
         * An integer holding the current double linked list size/lenght. This would be its 
         * element count number.
         */
        int _length;
        
        /**
         * Holds the current double linked list head Element object.
         */
        Element _head;
        
        /**
         * Search for an element on this double linked list.
         * 
         * @param elem       an element pointer to be found.
         * @return null the element is not found, otherwise an Element pointer to the found
         *         element.
         */
        Element* search( Element* elem );
        
    }; // end class Queue
    
} // end namespace BOOOS

#endif // QUEUE_H_













