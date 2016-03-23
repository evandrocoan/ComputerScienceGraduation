/*
 * Queue_Test.cc
 *
 */

#include <iostream>
#include "queue.h"
#include <string>

using namespace std;
using namespace BOOOS;

class MyElement: public Queue::Element
{
public:
    MyElement( string str ) :
            _name( str )
    {
    }
    
    virtual ~MyElement()
    {
    }
    
    string & name()
    {
        return _name;
    }
    
private:
    string _name;
};

void print_queue( Queue & q )
{
    cout << "Queue length: " << q.length() << endl;
    
    if ( q.length() )
    {
        MyElement * elem = dynamic_cast< MyElement * >( q.head()->next() );
        do
        {
            cout << elem->name() << endl;
            elem = dynamic_cast< MyElement * >( elem->next() );
        } while( elem != q.head()->next() );
    }
    
    cout << "===============Terminou um print queue===============" << endl;
}

int main()
{
    

    cout << "Welcome to BOOOS - Basic Object Oriented Operating System!"
    << endl;
    cout << "This program will test the class: Queue" << endl;
    
    Queue queue;
    
    MyElement * person1 = new MyElement( "Joao" );
    MyElement * person2 = new MyElement( "Pedro" );
    MyElement * person3 = new MyElement( "Augusto" );
    MyElement * person4 = new MyElement( "Fabio" );
    
    cout << "\nComecou Print 1\n" << endl;
    print_queue( queue );
    cout << "\nTerminou Print 1\n" << endl;
    
    queue.insert( person1 );
    queue.insert( person2 );
    queue.insert( person3 );
    queue.insert( person4 );
    
    cout << "\nComecou Print 2\n" << endl;
    print_queue( queue );
    cout << "\nTerminou Print 2\n" << endl;
    
    MyElement * removed_person = dynamic_cast< MyElement * >( queue.remove() );
    delete removed_person; // Which element was removed?
    
    cout << "\nComecou Print 3\n" << endl;
    print_queue( queue );
    cout << "\nTerminou Print 3\n" << endl;
    
    return 0;
}
