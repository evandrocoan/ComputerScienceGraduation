import java.awt.event.*;


class Paleta implements ActionListener {
  private Quadro quadro;
  Paleta(Quadro quadro) {
   this.quadro = quadro;
   editor = new EditorRetangulo(quadro);
  }

  private Editor editor;
  Editor editor() {
    return editor;
  }


  static String
    RETANGULO = "RETANGULO",
    CIRCULO = "CIRCULO";


   public void actionPerformed(ActionEvent e) {
      String cmd = e.getActionCommand();
      if (cmd.equals(RETANGULO)) {
          editor = new EditorRetangulo(quadro);
      }
      else if (cmd.equals(CIRCULO)) {
       //editor = new EditorCirculo(quadro);
      }
   }

}//
















