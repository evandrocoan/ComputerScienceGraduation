/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package Trabalho2;

/**
 *
 * @author Professional
 */
public class Ponto 
{
	private double abscissa;
	private double ordenada;
	
	// class's constructor
	public Ponto( double abscissa, double ordenada )
	{
		this.abscissa = abscissa;
		this.ordenada = ordenada;
		
	}

	/**
	 * @return the abscissa
	 */
	public double getAbscissa() {
		return abscissa;
	}

	/**
	 * @return the ordenada
	 */
	public double getOrdenada() {
		return ordenada;
	}

	/**
	 * @param abscissa the abscissa to set
	 */
	public void setAbscissa(double abscissa) {
		this.abscissa = abscissa;
	}

	/**
	 * @param ordenada the ordenada to set
	 */
	public void setOrdenada(double ordenada) {
		this.ordenada = ordenada;
	}
	
	public double distanciaEntrePontos_AB( Ponto outroPonto )
	{ 
		return ( 
				Math.sqrt( 
					Math.pow( 
								this.abscissa - outroPonto.getAbscissa() , 2
                             ) 
								+ 
					Math.pow( 
								this.ordenada - outroPonto.getOrdenada() , 2 
                             ) 
                        ) 
              );
				
	}
	
} // end class Ponto
