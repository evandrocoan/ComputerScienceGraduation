import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


class Quadro extends JPanel {
  private int[]
     x = new int[500], y = new int[500];//pontos centrais
  private int k = 0;

  private int raio = 20;
  
  void clique(int x, int y){//ponto central de um novo circulo
     this.x[k] = x;
     this.y[k] = y;
     k = k + 1;
  }
  int n = 0;
  public void paintComponent(Graphics g) { System.out.println("n = " + ++n);

     super.paintComponent(g);
     for(int i = 0; i < k; i++) {
	     int x0,y0;
	       x0 = x[i]-raio; 
	       y0 = y[i]-raio; 
	     g.drawOval(x0,y0,2*raio,2*raio);
     }
  }


}
