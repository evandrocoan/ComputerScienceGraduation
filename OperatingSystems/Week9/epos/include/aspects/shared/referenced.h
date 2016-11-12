#ifndef __referenced_h
#define __referenced_h

__BEGIN_SYS

class Referenced
{
protected:
  Referenced(): refs(1) {}
  int inc_refs() { return refs++; }
  int dec_refs() { return --refs; }

private:
  int refs;
};

__END_SYS

#endif
