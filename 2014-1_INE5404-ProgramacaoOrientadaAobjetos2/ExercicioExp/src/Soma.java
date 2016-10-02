class Soma implements Exp{
	private Exp numero1, numero2;
	Soma(Exp numero1, Exp numero2){
		this.numero1 = numero1;
		this.numero2 = numero2;
	}
	public double avaliar() {
		return numero1.avaliar() + numero2.avaliar();
	}
	public String toString(){
		//return " " + numero1 + " + " + numero2;
		return "Soma(" + numero1 + "," + numero2 + ")";
	}
}
