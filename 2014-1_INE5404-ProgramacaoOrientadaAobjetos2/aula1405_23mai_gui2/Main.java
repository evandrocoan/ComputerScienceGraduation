import java.awt.*;
import java.awt.event.*;
import javax.swing.*;


class Main {

  public static void main(String[] sds) {
    JFrame f = new JFrame();
    JPanel r0,r1,r2;
    f.setContentPane(r0 = new JPanel());
    r0.setLayout(new BorderLayout());
    r0.add(r1=new JPanel(),BorderLayout.NORTH);
    r0.add(r2=new JPanel(),BorderLayout.SOUTH);
    r1.setLayout(new FlowLayout());
    r2.setLayout(new FlowLayout());
 
    JButton b1,b2;
    r1.add(new JLabel("ola!"));
    r1.add(new JLabel("tudo bem?"));
    r2.add(b1=new JButton("mais"));
    r2.add(b2=new JButton("menos"));
    


    ActionListener t;
    t=new OuveBotoes();
    b1.addActionListener(t);    
    b2.addActionListener(t);    
    b1.setActionCommand("mais");
    b2.setActionCommand("menos");

    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    f.setSize(300,200);
    f.pack();
    f.setVisible(true);

  }

}//


class OuveBotoes implements ActionListener {
  public void actionPerformed(ActionEvent e) {
    //System.out.println("mais ou menos");
    String cmd = e.getActionCommand();
    System.out.println(cmd);
    
  }

}









