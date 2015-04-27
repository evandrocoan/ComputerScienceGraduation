/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Trabalho2;

import javax.swing.JOptionPane;

/**
 *
 * @author Professional
 */
public class Interface 
{
	
	public Ponto pecaDados( String pontoAtual )
	{
		double abscissa = 0;
		boolean isBoolean = true;
		
		while( isBoolean == true )
		{
			try
			{
            abscissa = Double.parseDouble( 
                  JOptionPane.showInputDialog( 
                  "Insira a abscissa " + pontoAtual + ":" ) );
            isBoolean = false;
			
			} catch( NumberFormatException instanceError ) 
			{
				JOptionPane.showMessageDialog(null, "Valor inválido " + 
						"inserido!\n" + instanceError.getMessage() );
				
			}
			
		}

		double ordenada = 0;
		isBoolean = true;
		
		while( isBoolean == true )
		{
			try
			{
            ordenada = Double.parseDouble( 
                  JOptionPane.showInputDialog( 
                  "Insira a ordenada " + pontoAtual + ":" ) );
            isBoolean = false;
			
			} catch( NumberFormatException instanceError ) 
			{
				JOptionPane.showMessageDialog(null, "Valor inválido " + 
						"inserido!\n" + instanceError.getMessage() );
				
			}
			
		}
		
		Ponto umPonto = new Ponto( abscissa, ordenada );
		
		return umPonto;
		
	}
	
	public void mostreResultado( double distanciaOrigem_umPonto, 
			double distanciaOrigem_outroPonto, double distanciaPontos,
			double abscissaUmPonto, double ordenadaUmPonto, 
			double abscissaOutroPonto, double ordenadaOutroPonto, 
			double abscissaOrigem, double ordenadaOrigem )
	{
		String text = String.format( "A distância do ponto um (%f, %f) " + 
				"até a origem (%f, %f) é: %f\nA distância do ponto outro" + 
				" (%f, %f) até a origem é: %f \nE a distância entre o ponto" + 
				" um e outro é: %f", 
				abscissaUmPonto, ordenadaUmPonto, 
				abscissaOrigem, ordenadaOrigem, 
				distanciaOrigem_umPonto, 
				abscissaOutroPonto, ordenadaOutroPonto, 
				distanciaOrigem_outroPonto, 
				distanciaPontos );
		
		JOptionPane.showMessageDialog(null, text );
		
	}
	
} // end class Interface
