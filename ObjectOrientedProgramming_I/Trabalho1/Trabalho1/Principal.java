/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Trabalho1;

/**
 *
 * @author Evandro  Coan
 */
public class Principal 
{
	// main method begins execution Java application
	public static void main( String[] args )
	{
		Interface aInterface = new Interface();
		
		double base = aInterface.pecaBase();
		double altura = aInterface.pecaAltura();
		
		Retangulo umRetangulo = new Retangulo( base, altura);

		double valorArea = umRetangulo.forneceArea();
		double perimetro = umRetangulo.fornecePerimetro();
		
		aInterface.mostreResultado( valorArea, base, altura, perimetro );
		
	} // end main method
	
} // end class Principal
