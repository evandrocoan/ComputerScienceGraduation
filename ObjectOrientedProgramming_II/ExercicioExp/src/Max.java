class Max extends Assoc{
	int bin(int x, int y) {
		if(x < y){
			return y;
		}
		return x;
	}
}
		
// Se for genérico: <E> E calcular(E[] a);