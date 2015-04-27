class Subtração {
	private Exp numero1, numero2;
	Subtração(Exp x, Exp y){
		numero1 = x;
		numero2 = y;
	}
	public double avaliar(){
		return numero1.avaliar() - numero2.avaliar();
	}
}
