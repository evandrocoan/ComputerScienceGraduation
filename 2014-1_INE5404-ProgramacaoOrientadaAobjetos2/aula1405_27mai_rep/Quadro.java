import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


class Quadro extends JPanel {
  private Reproduzivel[] fig = new Reproduzivel[500];//circulos
  private int k = 0;
  private Paleta paleta;
  Quadro(Paleta paleta) {
    this.paleta = paleta;
  }
  
  void clique(int x, int y){//ponto central de um novo circulo
     this.fig[k] = paleta.mkFig(x,y);
     k = k + 1;
  }
  


  int n = 0;
  public void paintComponent(Graphics g) { System.out.println("n = " + ++n);

     super.paintComponent(g);
     for(int i = 0; i < k; i++) {
         /* codigo em Ponto,Circulo,FigCirculo
	     int x0,y0;
	       x0 = x[i]-raio; 
	       y0 = y[i]-raio; 
	     g.drawOval(x0,y0,2*raio,2*raio); */

         fig[i].reproduzir(g);
     }
  }


}//
