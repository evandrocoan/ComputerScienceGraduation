import java.awt.*;
import javax.swing.*;


class Main {

  public static void main(String[] sds) {
    JFrame f = new JFrame();
    JPanel p;
    f.setContentPane(p = new JPanel());
    p.setLayout(new FlowLayout());//default para JPanel
 
    f.add(new JLabel("ola!"));
    f.add(new JLabel("tudo bem?"));
    f.add(new JLabel("mais ou menos."));
    
    f.setSize(300,200);
    f.setVisible(true);

  }



}
