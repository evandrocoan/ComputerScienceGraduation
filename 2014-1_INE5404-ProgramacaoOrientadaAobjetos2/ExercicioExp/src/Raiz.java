class Raiz {
	private Exp numero1;
	Raiz(Exp numero){
		numero1 = numero;
	}
	public double avaliar(){
		return Math.sqrt(numero1.avaliar());
	}
}
