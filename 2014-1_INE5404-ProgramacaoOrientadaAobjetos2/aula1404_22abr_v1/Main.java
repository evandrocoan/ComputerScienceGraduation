import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

class Main {

  public static void main(String[] _) {
    JFrame f = new JFrame();
    JPanel quadro = new Quadro();
    quadro.setPreferredSize(new Dimension(400,300));
    f.setContentPane(quadro);
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    //f.setSize(400,300);//lar x alt
    f.pack();
    f.setVisible(true);
    
    anima(quadro);

     
  }

  static void anima(JPanel quadro) {
     int n;
     for(;;) {
       quadro.repaint();
       try {
          Thread.currentThread().sleep(1000);
       } catch(Exception e){}
     }
  }


}//
