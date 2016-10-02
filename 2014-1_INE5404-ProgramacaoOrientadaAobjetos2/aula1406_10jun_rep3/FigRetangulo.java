

class FigRetangulo extends Retangulo implements Reproduzivel {
   FigRetangulo(int x0, int y0, int x1, int y1) {
    super(new Ponto(x0,y0),new Ponto(x1,y1));
   }

  public void reproduzir(java.awt.Graphics g) {
     g.drawRect(this.xesq(),this.ysup(),this.lar(),this.alt());
  } 

}//
