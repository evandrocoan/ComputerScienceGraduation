class Num implements Exp {
	private double numero;
	Num(double x){
		this.numero = x;
	}
	public double avaliar() {
		return numero;
	}
	public String toString(){
		return "" + numero;
	}
}
