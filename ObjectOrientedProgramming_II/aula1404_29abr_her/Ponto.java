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

  int distanciaX(Ponto q) {
    int lar = this.x - q.x;
    if(lar < 0) lar = -lar;
    return lar;
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
  

}
