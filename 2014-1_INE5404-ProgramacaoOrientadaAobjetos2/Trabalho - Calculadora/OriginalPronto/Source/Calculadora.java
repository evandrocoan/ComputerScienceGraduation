/* Calculadora.java
 * UFSC - Prof. Rosvelter J. Coelho da Costa
 *
 * Observar correspondencia com o diagrama de estados (cf. aula/transparencias).
 */

import javax.swing.*;
import java.awt.event.*;

class Main {
  public static void main(String[] args) { new Calculadora();}

}


interface OpBin {
  double bin(double x, double y);
}

interface Estado {

  void eval();//?'='
  void eval(char x);//?x='0'|..|'9'|','
  void eval(OpBin op);//?op='+'|'-'|'*'|'/'
}//



public class Calculadora implements ActionListener {
  private JTextField visor;
  String digs = "";
  double acc = 0.0, val = 0.0;
  OpBin op = null;
  Estado[] estado;
  int atual = 0;

 private final OpBin
  soma  = new OpBin() { public double bin(double x, double y) { return x+y;}},
  menos  = new OpBin() { public double bin(double x, double y) { return x-y;}},
  produto = new OpBin() { public double bin(double x, double y) { return x*y;}},
  divisao = new OpBin() { public double bin(double x, double y) { return x/y;}};


  Calculadora() {
   visor = new GUI(this).visor;
   op = soma;
 
   estado = new Estado[] {
    new CalculandoResultado(),
    new DigitandoNumero(),
    new EscolhendoOperacao()//, ...
   };
 
  }

  void exe()
  {
      acc = op.bin(acc,val);
      show(acc);
  }
  
  void show(double val) {
   //visor.setText("["+ estado + "] " + val);
   visor.setText("" + val);
  }

  void show(String val)
  {
   //visor.setText("["+ estado + "] " + val);
   visor.setText(val);
  }


//estado 0
class /*interna*/ CalculandoResultado implements Estado {
  public void eval() {//?'='
   //execute com os mesmos operando e operacao  ...
    exe();
  }
  public void eval(char x) {//?x='0'|..|'9'|','
    acc = 0.0;
    op = soma;
    digs = "" + x;
    show(digs);
    atual = 1;
  }
  public void eval(OpBin s) {//?s='+'|'-'|'*'|'/'
   val = acc;
   op = s;
   atual = 2;
  }
}//interna


//estado 1
class /*interna*/ DigitandoNumero implements Estado {
  public void eval() {//?'='
   try { val = Double.parseDouble(digs);} catch (Exception ignorada) {}
  exe();
   atual = 0;
  }
  public void eval(char x) {//?x='0'|..|'9'|','
   digs += x;
   show(digs);
 }
  public void eval(OpBin s) {//?s='+'|'-'|'*'|'/'
   try { val = Double.parseDouble(digs);} catch (Exception ignorada) {
   }//experimente tratar! (exercicio)
   exe();
   op = s;
   atual = 2;
  }
}//interna


//estado 2
class /*interna*/ EscolhendoOperacao implements Estado {
  public void eval() {//?'='
   exe();
   atual = 0;
  }
  public void eval(char x) {//?x='0'|..|'9'|','
   digs = "" + x;
   show(digs);
   atual = 1;
  }
  public void eval(OpBin s) {//?s='+'|'-'|'*'|'/'
    //proxima operacao ...
    op = s;
  }
}//interna



  //Definindo o metodo actionPerformed da interface 'ActionListener' ...
  //Obs. Mesmo os condicionais deste metodo podem ser evitados.
  public void actionPerformed(ActionEvent e) {
   
   String tecla = e.getActionCommand();
    
   if (tecla.charAt(0) == '$') {//se eh um digito ...
     estado[atual].eval(tecla.charAt(1));
   }
   else if (tecla.equals("SEPARADOR_DECIMAL")) {
     estado[atual].eval('.');
   }
   else if (tecla.equals("SOMA")) {
     estado[atual].eval(soma);
   }
   else if (tecla.equals("MENOS")) {
     estado[atual].eval(menos);
   }
   else if (tecla.equals("PRODUTO")) {
     estado[atual].eval(produto);
   }
   else if (tecla.equals("DIVISAO")) {
     estado[atual].eval(divisao);
   }
   else if (tecla.equals("RESULTADO")) {
     estado[atual].eval();
   }

  }

}//Calculadora

