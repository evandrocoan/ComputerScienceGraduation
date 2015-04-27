class Derivada implements Exp{
	private Exp polinomio;
	Derivada(Exp polinomio){
		this.polinomio = polinomio;
	}
	public double avaliar() {
		return polinomio.avaliar() + polinomio.avaliar();
	}
	public String toString(){
		//return " " + numero1 + " + " + numero2;
		return "Derivada(" + polinomio + ")";
	}
}