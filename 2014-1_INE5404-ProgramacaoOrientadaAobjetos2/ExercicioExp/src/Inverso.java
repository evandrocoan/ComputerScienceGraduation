class Inverso implements Exp {
	private Exp numero1;
	Inverso(Exp numero){
		numero1 = numero;
	}
	public double avaliar(){
		return 1/numero1.avaliar();
	}
}
