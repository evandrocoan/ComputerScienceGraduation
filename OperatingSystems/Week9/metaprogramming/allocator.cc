template<int n_res, class Resource>
class Allocator
{
public:
  Allocator() {
    for(int i = 0; i < n_res; i++) used[i] = false;
  }; 

  Resource *alloc() {
    int i;
    for(i = 0; (i < n_res) && used[i]; i++);
    return (i == n_res)?0:(used[i] = true, &resource[i]);
  }

  void free(Resource *res) {
    int i;
    for(i = 0; (i < n_res) && (&resource[i] != res); i++);
    if(i != n_res)
      used[i] = false;
  }

private:
  bool used[n_res];
  Resource resource[n_res];
};

#define n_bufs 16
typedef char Buffer[256];
Allocator<n_bufs, Buffer> buffer_allocator;

int main()
{
  Buffer *buf[n_bufs + 1];

  int i = 0;
  do {
    buf[i] = buffer_allocator.alloc();
  } while(buf[i++]);

  while(i > 0)
    buffer_allocator.free(buf[--i]);
}
