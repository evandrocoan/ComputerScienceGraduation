class Polinomio implements Exp{
	private Exp e;
	private double[] variavel;
	Polinomio(double[] coeficientes, Exp e){
		this.variavel = coeficientes;
		this.e = e;
		}
	public double avaliar(){
		double v = e.avaliar();
		double s = 0;
		int n = variavel.length;
		for(int i = 0; i < n; i++){
			s += variavel[i] * Math.pow(v, i);
		}
		return s;
	}
}
