import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


class Quadro extends JPanel {
  private int
     x = 80,
     x1 = 10,
     x2 = 210;
  private int d = 1;
  public void paintComponent(Graphics g) {
     super.paintComponent(g);
     
     g.fillRect(x1,10,2,50);
     g.fillRect(x2,10,2,50);
 
     //cond: indo para a dir
     if (x > x2) d = -d;//inv ok

     //cond: indo para a esq
     if (x < x1) d = -d;//inv ok


     x = x+ d*5; System.out.println(""+x);

     g.drawRect(x,20,30,30);
  }


}
