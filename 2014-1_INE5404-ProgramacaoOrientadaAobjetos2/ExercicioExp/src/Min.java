class Min extends Assoc{
		int bin(int x, int y) {
			if(x >= y){
				return y;
			}
			return x;
		}
	}