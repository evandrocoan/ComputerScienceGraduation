/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Trabalho1;

import javax.swing.JOptionPane;

/**
 *
 * @author Evandro  Coan
 */
public class Interface 
{
	double base, altura;
	String text, // texto inserido pelo usuário
			text2; // texto (mensagem) enviada (mostrada) ao usuário
	
	public double pecaBase()
	{
		text = JOptionPane.showInputDialog( "Insira a base: " );
		base = Double.parseDouble(text);
		return  base;
		
	}
	
	public double pecaAltura() 
	{
		text = JOptionPane.showInputDialog( "Insira a base: " );
		altura = Double.parseDouble(text);
		return  altura;
		
	}
	
	public void mostreResultado( double valorArea, double base,
			double altura, double perimetro )
	{
		text2 = String.format( "O valor da área é: %f \nE sua base é: %f" + 
				"\nE sua altura é: %f\nE o seu perímetro é: %f", 
				valorArea, base, altura, perimetro );
		JOptionPane.showMessageDialog(null, text2 );
		
	}
	
} // end class Interface
