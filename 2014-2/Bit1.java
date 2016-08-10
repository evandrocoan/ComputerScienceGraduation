import javax.swing.JOptionPane;
public class Bit1
{
	public static void main(String[] par)
	{
		float  seuFloat  = -1e37f;
		double seuDouble = -1e37d,erro;

		int intBits = Float.floatToIntBits(seuFloat); 
		String binario = Integer.toBinaryString(intBits);
		//JOptionPane.showMessageDialog(null, String.format("O valor binario de %.20e eh: ",(float)seuFloat)+ " "+ binario);
		System.out.printf("\n Valor binario: %s",binario);
                System.out.println(String.format("O valor binario de \n%.30e eh: ",(float)seuFloat));
                System.out.println(String.format("O valor binario de \n%.30e eh: ",(double)seuDouble));
		erro=((seuFloat-seuDouble)/seuDouble)*100.;
		//JOptionPane.showMessageDialog(null, String.format("O erro float de %.20f eh: %e %%",(float)seuFloat,(double)erro));
		System.out.println(erro);
	
		
	}
}
