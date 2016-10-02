import static java.lang.Math.*;

class Ponto {
  private int x,y;

  Ponto(int x, int y) {
    //super();//default: Object()
    this.x = x;
    this.y = y;
  }
  Ponto(int z) {
    this(z,z);
  }

  void deslocarX(int val) { this.x = this.x + val;}
  void deslocarY(int val) { this.y = this.y + val;}

  public String toString() {
    return "(" + x + "," + y + ")";
  }

  int xesq(Ponto q) {
    int x0 = this.x;
    if(q.x < x0) return q.x;
    return x0;
  }

  int ysup(Ponto q) {
    int y0 = this.y;
    if(q.y < y0) return q.y;
    return y0;
  }


  int distanciaX(Ponto q) {
    int lar = this.x - q.x;
    if(lar < 0) lar = -lar;
    return lar;
  }

  int distanciaY(Ponto q) {
    int alt = this.y - q.y;
    if(alt < 0) alt = -alt;
    return alt;
  }


  double distancia() {//norma
    return this.distancia(new Ponto(0,0));
  }

  double distancia(Ponto q) {
    long
     deltaX = this.x - q.x, 
     deltaY = this.y - q.y;
     return sqrt(pow(deltaX,2) + pow(deltaY,2));
     
  }
  
  int x0(int raio) {
    return x-raio;
  }

  int y0(int raio) {
    return y-raio;
  }

}
