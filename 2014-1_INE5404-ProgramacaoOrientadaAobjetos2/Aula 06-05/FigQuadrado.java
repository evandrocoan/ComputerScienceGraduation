class FigQuadrado implements Reproduzivel {
	private int x,y, lado;
	FigQuadrado(int x, int y, int lado) {
		this.x = x;
		this.y = y;
		this.lado = lado;
	}
	public void reproduzir(java.awt.Graphics g){
	     	g.drawRect(this.x,this.y,this.lado,this.lado);	
	}
}

