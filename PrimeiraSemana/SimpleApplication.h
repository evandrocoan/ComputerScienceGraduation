#ifndef __simpleapplication_h
#define __simpleapplication_h
using namespace std;

class SimpleApplication
{
    public:
        SimpleApplication()
        {
            cout << "Hello World! I'm the SimpleApplication "
                    "Constructor" << endl;
        }
        void run();
    private:
};

#endif
