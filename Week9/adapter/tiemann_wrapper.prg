class A_Class
{
public:
    int ()A_Class(int(A_Class::* function)(int), int i) {
	enter();
	int ret = (this->*function)(i);
	leave();
	return ret;
    }

    int operation(int);
}

A_Class* p = new A_Class;
p->operation(1);  // p->()A_Class(&A_Class::operation, 1);
