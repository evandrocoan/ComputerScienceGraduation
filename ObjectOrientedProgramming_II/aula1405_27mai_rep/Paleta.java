import java.awt.event.*;


class Paleta implements ActionListener {
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
















