abstract class Assoc {
	int calcular(int[] a){ // |a| > 0
		int x = a[0];
		int n = a.length;
		for(int i = 0; i < n; i++){
			x = bin(x, a[i]);
		}
		return x;
	}
	abstract int bin(int x, int y);
}