import javax.swing.JOptionPane;
public class Bit2
{
	public static void main(String[] par)
	{
		double h, x, y, e, f, g;

		h = 1/2; 
		x = 2/3 -h; 
		y = 3/5 - h; 
		e = (x + x + x) - h;
		f = (y + y + y + y + y) - h; 
		g = e/f;

		System.out.printf( "\n e = %.200f ", e );
		System.out.printf( "\n f = %.200f ", f );
		System.out.printf( "\n g = %.200f \n", g );
		
	}
}
