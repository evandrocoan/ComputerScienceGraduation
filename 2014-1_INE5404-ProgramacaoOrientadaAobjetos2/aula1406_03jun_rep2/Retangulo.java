

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

  int xesq() {
    return a.xesq(b);
  }
  int ysup() {
    return a.ysup(b);
  }


  int lar() {//encapsulacao OK!
    return a.distanciaX(b);
  }

  int alt() {//encapsulacao OK!
    return a.distanciaY(b);
  }


}
