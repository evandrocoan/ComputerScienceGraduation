import java.awt.event.*;


class Paleta implements ActionListener {
  private Quadro quadro;
  Paleta(Quadro quadro) {
   this.quadro = quadro;
  }

  void clique(int x, int y){//ponto central de um novo circulo
     quadro.addFig(this.mkFig(x,y));
     quadro.repaint();
   }
  
  static String
    RETANGULO = "RETANGULO",
    CIRCULO = "CIRCULO";

  private String tipo = "RETANGULO";
  private int raio = 20;


   public void actionPerformed(ActionEvent e) {
      String cmd = e.getActionCommand();
      if (cmd.equals(RETANGULO)) {
         tipo = RETANGULO;
      }
      else if (cmd.equals(CIRCULO)) {
         tipo = CIRCULO;
      }
   }
   Reproduzivel mkFig(int x, int y){
     if(tipo.equals(RETANGULO)) {
       return new FigQuadrado(x,y,raio);
     }
     else if(tipo.equals(CIRCULO)) {
       return new FigCirculo(x,y,raio);
     }
     return new FigQuadrado(x,y,raio);
   }


}
















