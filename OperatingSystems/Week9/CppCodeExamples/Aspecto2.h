
#ifndef ASPECTO2_H
#define ASPECTO2_H

namespace OS {

    class Aspecto2 {
    public:
        Aspecto2();
        Aspecto2(const Aspecto2& orig);
        virtual ~Aspecto2();
    public:
        void enter();
        void leave();
    private:

    };
}
#endif /* ASPECTO2_H */

