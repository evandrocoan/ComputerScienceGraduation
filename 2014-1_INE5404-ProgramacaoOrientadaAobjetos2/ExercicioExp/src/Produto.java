class Produto implements Exp{
	private Exp numero1, numero2;
	Produto(Exp numero1, Exp numero2){
		this.numero1 = numero1;
		this.numero2 = numero2;
	}
	public double avaliar() {
		return numero1.avaliar() * numero2.avaliar();
	}
	public String toString(){
			//return " " + numero1 + " + " + numero2;
			return "Produto(" + numero1 + "," + numero2 + ")";
		}
	}

/*Class Produto extends Assoc{
 * int bin(in x, int y){
 * 	return x * y;
 * 	}
 * }
 * 
 *  h1: P({z}) = z;
h2: P(a + z) = bin(P(a),z);
n! = 1*2*3*.....*n;
	=*{1,2,3,;;;,n};
1! = *{1} = 1;
(n+1)! = n!*n+1 = *{1, 2, 3, ...., n} * n+1;
						P(a);             z;
*/

