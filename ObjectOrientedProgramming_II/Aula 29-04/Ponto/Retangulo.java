

class Retangulo {
  private Ponto a,b;
  Retangulo(Ponto a, Ponto b) {
    this.a = a;
    this.b = b;

  }

  void deslocarX(int val) {
    a.deslocarX(val);
    b.deslocarX(val);
  }
  void deslocarY(int val) {
    a.deslocarY(val);
    b.deslocarY(val);
  }
/* 
  int lar() {//foi-se a encapsulacao!
    int lar = a.x - b.x;
    if(lar < 0) lar = -lar;
    return lar;
  }
*/
  int lar() {//encapsulacao OK!
    return a.distanciaX(b);
  }



}
