import java.awt.*;
import java.awt.event.*;
import javax.swing.*;


class Main {

  public static void main(String[] sds) {
    JFrame f = new JFrame();
    JPanel r0,r1;
    f.setContentPane(r0 = new JPanel());
    r0.setLayout(new BorderLayout());
    JLabel l;
    l=new JLabel("0");
    r0.add(l,BorderLayout.NORTH);
    r0.add(r1=new JPanel(),BorderLayout.SOUTH);
    r1.setLayout(new FlowLayout());
 
    JButton b1,b2,b3;
    r1.add(b1=new JButton("+"));
    r1.add(b2=new JButton("-"));
    r1.add(b3=new JButton("zerar"));
    


    ActionListener t;
    t=new Contador(l);
    b1.addActionListener(t);    
    b2.addActionListener(t);    
    b3.addActionListener(t);    
    b1.setActionCommand("INCREMENTAR");
    b2.setActionCommand("DECREMENTAR");
    b3.setActionCommand("ZERAR");

    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    f.setSize(300,200);
    f.pack();
    f.setVisible(true);

  }

}//


class Contador implements ActionListener {
  private int n = 0;//contagem
  private JLabel l;
  Contador(JLabel l) {
    this.l = l;
  }
  public void actionPerformed(ActionEvent e) {
    String cmd = e.getActionCommand();
    System.out.println(cmd);
    if(cmd.equals("INCREMENTAR")) {
      n = n + 1;
      l.setText(""+n);
    }
    
  }

}









