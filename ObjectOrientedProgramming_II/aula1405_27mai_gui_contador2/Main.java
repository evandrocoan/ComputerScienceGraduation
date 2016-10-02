import java.awt.*;
import java.awt.event.*;
import javax.swing.*;


class Main {

  public static void main(String[] sds) {
    JFrame f = new JFrame();
    JPanel r0,r1,r2;
    f.setContentPane(r0 = new JPanel());
    r0.setLayout(new BorderLayout());
    JLabel l;
    l=new JLabel("0");
    r0.add(l,BorderLayout.NORTH);
    r0.add(r1=new JPanel(),BorderLayout.CENTER);
    r0.add(r2=new JPanel(),BorderLayout.SOUTH);
    r1.setLayout(new FlowLayout());
    r2.setLayout(new FlowLayout());
 
    JButton b1,b2,b3;
    r1.add(b1=new JButton("+"));
    r1.add(b2=new JButton("-"));
    r1.add(b3=new JButton("zerar"));
    


    ActionListener t;
    t=new Contador(l);
    b1.addActionListener(t);    
    b2.addActionListener(t);    
    b3.addActionListener(t);    
    b1.setActionCommand(Contador.incrementar);
    b2.setActionCommand(Contador.decrementar);
    b3.setActionCommand(Contador.zerar);

    JButton b;
    r2.add(b=new JButton("MS"));
    b.addActionListener(t);    
    b.setActionCommand(Contador.salvar);
    r2.add(b=new JButton("MR"));
    b.addActionListener(t);    
    b.setActionCommand(Contador.carregar);
    




    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    f.setSize(300,200);
    f.pack();
    f.setVisible(true);

  }

}//


class Contador implements ActionListener {
  static /*final*/
  String
  //geralmente usa-se 'int' (menos memoria e mais rapido)
    incrementar = "INCREMENTAR",
    decrementar = "DECREMENTAR",
    zerar = "ZERAR",
    salvar = "SALVAR",
    carregar = "CARREGAR";
  private int
     n = 0,//contagem
     mem = 0;
  private JLabel l;
  Contador(JLabel l) {
    this.l = l;
  }
  public void actionPerformed(ActionEvent e) {
    String cmd = e.getActionCommand();
    System.out.println(cmd);
    if(cmd.equals(Contador.incrementar)) {
      n = n + 1;
      l.setText(""+n);
    }
    else if(cmd.equals(Contador.decrementar)) {
      n = n - 1;
      l.setText(""+n);
    }
    else if(cmd.equals(Contador.salvar)) {
      mem = n;
    }
    else if(cmd.equals(Contador.carregar)) {
      n = mem;
      l.setText(""+n);
    }
    
  }

}









