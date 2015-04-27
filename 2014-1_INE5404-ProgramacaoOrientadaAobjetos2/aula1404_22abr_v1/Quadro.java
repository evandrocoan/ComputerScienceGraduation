import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


class Quadro extends JPanel {
  private int x = 80;
  public void paintComponent(Graphics g) {
     super.paintComponent(g);
     g.drawRect(x,20,30,30);
    
     x = x +5; System.out.println(""+x);
  }


}
