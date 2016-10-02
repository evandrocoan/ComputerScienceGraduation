import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


class Main implements ActionListener {
  static JPanel quadro;
  public static void main(String[] _) {
    JFrame f = new JFrame();
    quadro = new Quadro();
    quadro.setPreferredSize(new Dimension(400,300));
    f.setContentPane(quadro);
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    //f.setSize(400,300);//lar x alt
    f.pack();
    f.setVisible(true);
    
    anima();

     
  }

  static void anima() {
      Timer t = new Timer(500, new Main());
      t.start();
     
  }

  public void actionPerformed(ActionEvent e) {
     quadro.repaint();
  }

}//
