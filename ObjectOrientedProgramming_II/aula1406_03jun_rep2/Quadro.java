import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


class Quadro extends JPanel {
  private Reproduzivel[] fig = new Reproduzivel[500];//circulos
  private int k = 0;
  
  void addFig(Reproduzivel fig) {
    this.fig[k] = fig;
     k = k + 1;
  }


  int n = 0;
  public void paintComponent(Graphics g) { System.out.println("n = " + ++n);

     super.paintComponent(g);
     for(int i = 0; i < k; i++) {
         fig[i].reproduzir(g);
     }
  }


}//
