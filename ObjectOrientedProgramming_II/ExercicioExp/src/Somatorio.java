class Somatorio implements Exp {
	private Exp[] somatorio;
	Somatorio(Exp[] valores){
			somatorio = valores.clone();
		}
	public double avaliar(){
		double soma = somatorio[0].avaliar();
		for(int j = 1; j < somatorio.length; j++){
			soma += somatorio[j].avaliar();
		}
		return soma;
	}
}
