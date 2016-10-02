

class Circulo extends Ponto {
  private int raio;

  Circulo(int x, int y, int raio) {
    super(x,y);
    this.raio = raio;
  }

  int diametro() {
    return 2*raio;
  }

}
