import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


class Main implements MouseListener {
  static Quadro quadro;
  public static void main(String[] _) {
    teste1();
    Circulo p = new Circulo(3,4,8);
    System.out.println(""+p.toString());

  }

  static void teste1() {
    JFrame f = new JFrame();
    Main.quadro = new Quadro();
    quadro.setPreferredSize(new Dimension(400,300));
    f.setContentPane(quadro);
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    //f.setSize(400,300);//lar x alt
    f.pack();
    f.setVisible(true);
    
    desenharCirculo();
  }

  static void desenharCirculo() {
    Main.quadro.addMouseListener(new Main());
  }

  public void mouseClicked (MouseEvent e) {
    int
      x = e.getX(),
      y = e.getY();
    System.out.println("("+ x + "," + y + ")");
    Main.quadro.clique(x,y);
    Main.quadro.repaint();
  }

  public void mousePressed (MouseEvent e) {}
  public void mouseReleased (MouseEvent e) {}
  public void mouseEntered (MouseEvent e) {}
  public void mouseExited (MouseEvent e) {}


}//
