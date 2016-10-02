/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Trabalho1;

/**
 *
 * @author Evandro  Coan
 */
public class Retangulo 
{
	private double base, altura; // atributos
	
	public Retangulo()
	{
		base = 0.0;
		altura = 0.0;
	}
	
	public Retangulo( double base, double altura )
	{
		this.base = base;
		this.altura = altura;
	}
	
	public void altereBase( double base )
	{
		this.base = base;
	}
	
	public void altereAltura( double altura )
	{
		this.altura = altura;
	}
	
	public double informeBase()
	{
		return base;
	}
	
	public double informeAltura()
	{
		return altura;
	}
	
	public double forneceArea()
	{
		return base * altura;
	}
	
	public double fornecePerimetro()
	{
		return 2 * base + 2 * altura;
	}
	
} // end class Retangulo
