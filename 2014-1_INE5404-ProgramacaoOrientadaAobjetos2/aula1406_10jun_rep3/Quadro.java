<html>import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;


class Quadro extends JPanel {
  //private Reproduzivel[] fig = new Reproduzivel[500];//circulos
  //private int k = 0;
  
  java.util.List<Reproduzivel> fig = new ArrayList<Reproduzivel>();
  
  void addFig(Reproduzivel fig) {
    //this.fig[k] = fig;
    //k = k + 1;
    this.fig.add(fig);
  }

  void removeFig(Reproduzivel fig) {
    //this.fig[k] = fig;
    //k = k + 1;
    this.fig.remove(fig);
  }



  int n = 0;
  public void paintComponent(Graphics g) { System.out.println("n = " + ++n);

     super.paintComponent(g);
     for(int i = 0; i < fig.size(); i++) {
         //fig[i].reproduzir(g);
         fig.get(i).reproduzir(g);
     }
  }


}//
