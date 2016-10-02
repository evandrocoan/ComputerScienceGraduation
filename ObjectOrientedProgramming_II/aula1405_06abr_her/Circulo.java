

class Circulo extends Ponto {
  private int raio;

  Circulo(int x, int y, int raio) {
    super(x,y);
    this.raio = raio;
  }

  int diametro() {
    return 2*raio;
  }

  int x0() {
     return this.x0(raio);
  }
  int y0() {
     return this.y0(raio);
  }

}//
