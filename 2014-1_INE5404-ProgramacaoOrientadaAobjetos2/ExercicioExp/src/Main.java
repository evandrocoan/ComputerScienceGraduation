class Main {
	public static void main(String[] par){
		Exp e, n2, n3, a23;
		n2 = new Num(2);
		n3 = new Num(3);
		a23 = new Soma(n2, n3);
		String s = "", sm = "", sm2 = "";
		s += n2;
		sm += a23 + " = " + a23.avaliar() + "\n"; //irá transformar a23 "automaticamente" pro toString;
		
		Exp p5a23 = new Produto(new Num(5), a23);
		sm2 += p5a23 + " = " + p5a23.avaliar();
		
		Exp[] valores = {new Num(3), new Num(4), new Num(5)};
		Exp somatorio = new Somatorio(valores);
		String soma ="" +  somatorio.avaliar();
		
		Exp inverso = new Inverso(new Num(5));
		String inversoo = "" + inverso.avaliar();
		e = new Soma(
				new Num(2),
				new Produto(
					new Num(5),
					new Num(4)
					)
				);
		
		/////////////////////////
		Exp p, v = new Num(3);
		p = new Polinomio(new double[]{1, 0, 3, -4}, v);
		double pv = p.avaliar();
		System.out.println("pv" + pv);
		/////////////////////////
		int[] a = {-2, 0, 1, 4};
		int max = new Max().calcular(a), min = new Min().calcular(a);
		double r = e.avaliar();
		System.out.println("Max = " + max + " e " + "Min = " + min);
		System.out.println("A resposta é:" + r);
		System.out.println(s);
		System.out.println(sm);
		System.out.println(sm2);
		System.out.println(soma);
		System.out.println(inversoo);
		
		
		////////////////////////////////////////////////////////////////////
	/*int fat(int n){
		int[] a = new int[n];
		for(int i = 0; i < n; i++){
			a[i] = i +1;
			
		}
		return new Produto().calcular(a);


	}*/
	//POLINOMIO, DERIVADA DE POLINOMIO; cociente.
	//SEGUNDO EXERCÍCIO: Fatorial
	}
}
