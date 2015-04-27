
class EditorRetangulo implements Editor {
  private Quadro quadro;
  EditorRetangulo(Quadro quadro) {
   this.quadro = quadro;
  }

  int x0,yo;
  int npontos = 0;
  public void clique(int x, int y) {
     quadro.addFig(new FigQuadrado(x,y,50));
     quadro.repaint();
  }

}//
