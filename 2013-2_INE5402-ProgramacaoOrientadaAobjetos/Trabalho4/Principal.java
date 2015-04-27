/*
 * Escreva um programa que calcule os valores de seno e cosseno para 
 *   angulos de 0.0, até 90.0 graus, com intervalo de 1.0 grau.
 * Um ângulo deve ser tratado como um objeto. Os valores de seno e 
 *   cosseno devem ser calculados pelas séries asseguir.
 * 
 * Onde x representa o valor do ângulo em radianos. 
 *   sin(x) = x - x^3/3! + x^5/5! - x^7/7! ... (-1)^(n+1)x^n/n!
 *   cos(x) = 1 - x^2/2! + x^4/4! - x^6/6! ... (-1)^(n+1)x^n/n!
 * 
 * public class Angulo
 * {
 *     private double graus;
 *     
 *     public double fornecaRadianos()
 *     {
 *         return graus * Math.PI/180;
 * 
 *     } // retorna o valor em radianos
 * 
 * }
 */

package Trabalho4;

/**
 * Manipular os objetos da classe Interface e Angulo
 * @author Evandro  Coan
 */
public class Principal 
{
	/**
    * Main method begins execution Java application
    * @param args 
    */
   public static void main( String[] args )
	{
      // cria um objeto da classe Interface
		Interface a = new Interface();
      
      // cria o objeto da classe Angulo
		Angulo b = new Angulo( a.inputAngle() );
      
      // obtém a precisão dos calculos do usuário
      int precisao = a.inputPrecision();
      
      // calcula e exibe o valor do seno e cosseno do objeto a
      a.showMessage( "O seno do ângulo inserido é: " + b.sin( precisao ) + 
              "\nE seu cosseno é: " + b.cos( precisao ) );
      
	} // end main method
	
} // end class Principal
