

class FigQuadrado extends Retangulo implements Reproduzivel {
   FigQuadrado(int x, int y,int lado) {
    super(new Ponto(x,y),new Ponto(x+lado,y+lado));
   }

  public void reproduzir(java.awt.Graphics g) {
     g.drawRect(this.xesq(),this.ysup(),this.lar(),this.alt());
  } 

}//
