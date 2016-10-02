import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


class Main implements MouseListener,ActionListener {
  static Quadro quadro;
  static Paleta paleta;
  public static void main(String[] _) {
    teste2();
  }

  static void teste2() {
    JFrame f = new JFrame();
    Main.quadro = new Quadro();
    Main.paleta = new Paleta(Main.quadro);
    f.setContentPane(quadro);

    JMenuBar barra = new JMenuBar();
    f.setJMenuBar(barra);
    
    JMenu guia;
    guia = new JMenu("Arquivo");
    barra.add(guia);

    JMenuItem item;
    item = new JMenuItem("Sair");
    guia.add(item);
    item.addActionListener(new Main());

    guia = new JMenu("Paleta");
    barra.add(guia);
    item = new JMenuItem("retangulo");
    item.setActionCommand(Paleta.RETANGULO);
    guia.add(item);
    item.addActionListener(paleta);
    item = new JMenuItem("circulo");
   item.setActionCommand(Paleta.CIRCULO);
    guia.add(item);
    item.addActionListener(paleta);

    



    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    f.setSize(400,300);//lar x alt
    //f.pack();
    f.setVisible(true);
    
    desenhar();
  }
   
   public void actionPerformed(ActionEvent e) {
      System.exit(0);
   }

 
  static void desenhar() {
    Main.quadro.addMouseListener(new Main());
  }

  public void mouseClicked (MouseEvent e) {
    int
      x = e.getX(),
      y = e.getY();
    System.out.println("("+ x + "," + y + ")");
    Main.paleta.clique(x,y);
  }

  public void mousePressed (MouseEvent e) {}
  public void mouseReleased (MouseEvent e) {}
  public void mouseEntered (MouseEvent e) {}
  public void mouseExited (MouseEvent e) {}


}//
