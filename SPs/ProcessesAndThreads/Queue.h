/*
 * BOOOS.h
 *
 */

#ifndef QUEUE_H_
#define QUEUE_H_

namespace BOOOS {

        class Queue {
        public:
                Queue();
                virtual ~Queue();

                class Element {
                public:
                        Element() { _prev = 0; _next = 0; _rank = 0; }
                        virtual ~Element() {}

                        Element * prev() { return _prev; }
                        Element * next() { return _next; }
                        int rank() { return _rank; }
                        void prev(Element * p) { _prev = p; }
                        void next(Element * p) { _next = p; }
                        void rank(int r) { _rank = r; }

                private:
                        Element * _prev;
                        Element * _next;
                        int _rank;
                };

                Element * head() { return &_head; }

                int length() { return _length; }

                void insert(Element * elem);
                void insert_ordered(Element * elem);

                Element * remove();
                void remove(Element * e);

        private:
                Element * search(Element * elem);

                Element _head; // _head.next will point to head, _head.prev will point to tail
                int _length;
        };

}

#endif /* QUEUE_H_ */
