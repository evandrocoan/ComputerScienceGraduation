import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


class Quadro extends JPanel {
  private int
     x,y,//ponto central
     raio = 20;
  
  void clique(int x, int y){
     this.x = x;
     this.y = y;
  }
  public void paintComponent(Graphics g) {
     super.paintComponent(g);

     int x0,y0;
       x0 = x-raio; 
       y0 = y-raio; 
     g.drawOval(x0,y0,2*raio,2*raio);
  }


}
