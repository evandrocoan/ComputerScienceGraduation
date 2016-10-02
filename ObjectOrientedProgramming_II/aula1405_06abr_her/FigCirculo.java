

class FigCirculo extends Circulo {
 
  FigCirculo(int x, int y, int raio) {
    super(x,y,raio);
  }

  void reproduzir(java.awt.Graphics g) {
     g.drawOval(x0(),y0(),this.diametro(),this.diametro());
  } 

}//
