public class teste
{
public static void main(String[] par)
{
float seuFloat = -11.7e-0f;
double seuDouble = -11.7e-0d,erro;
int intBits = Float.floatToIntBits(seuFloat); 
String binario = Integer.toBinaryString(intBits);
System.out.println(String.format("O valor binario de %.20f é: \n",(float)seuFloat)+binario);
erro=((seuFloat-seuDouble)/seuDouble)*100.;
System.out.println(String.format("O erro float de %.20f é: \n%e%%",(float)seuFloat,(double)erro));
}
}

// /home/100000000393190/.config/sublime-text-3/Packages/User
