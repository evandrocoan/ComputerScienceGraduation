
#ifndef ASPECTO1_H
#define ASPECTO1_H

namespace OS {

    class Aspecto1 {
    public:
        Aspecto1();
        Aspecto1(const Aspecto1& orig);
        virtual ~Aspecto1();
    public:
        void enter();
        void leave();
    private:

    };
}
#endif /* ASPECTO1_H */

