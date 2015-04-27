
class EditorRetangulo implements Editor {
  private Quadro quadro;
  EditorRetangulo(Quadro quadro) {
   this.quadro = quadro;
  }

  int x0,y0;
  int npontos = 0;
  Reproduzivel figtmp;
  
  public void clique(int x, int y) {
    if (npontos == 0) {
       x0 = x;
       y0 = y;
       npontos = 1;
       quadro.addFig(figtmp=new FigCirculo(x,y,5));
       quadro.repaint();
       return;
     }
    if (npontos == 1) {
        npontos = 0;
       quadro.removeFig(figtmp);
       quadro.addFig(new FigRetangulo(x0,y0,x,y));
       quadro.repaint();
       return;
     }
  }

}//
