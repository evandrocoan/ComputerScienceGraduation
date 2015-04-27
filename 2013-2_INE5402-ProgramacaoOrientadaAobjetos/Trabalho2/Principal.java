/*
 * Considere um ponto representado no sistema de coordenadas cartesianas.
 * Escreva um programa que:
 *  a) Calcule a distância entre dois pontos.
 *  b) Para cada ponto do item a, determine sua distância até o ponto 
 *	de origem.
 * Obs: 1) Um ponto deve ser tratado com um objeto.
 * 2) Um ponto deve ter a capacidade de determinar a sua distância até 
 *  outro ponto.
 * 3) São três objetos, pontoA, pontoB, pontoO (origem), da classe Pontos, 
 *  com atributos, abscissa e ordenada.
 * 4) A classe ponto deve ter um método que o objeto ao executar o método 
 *  retorne a distância entre pontos.
 * 5) Construtor, métodos de acesso (para os atributos private).
 */

package Trabalho2;

/**
 *
 * @author Professional
 */
public class Principal 
{
	// main method begins execution Java application
	public static void main( String[] args )
	{
		Interface aInterface = new Interface();
		
		Ponto umPonto = aInterface.pecaDados( "do Ponto um" );
		Ponto outroPonto = aInterface.pecaDados( "do Ponto outro" );
		Ponto origemPonto = aInterface.pecaDados( "da Origem" );
		
		aInterface.mostreResultado( 
				umPonto.distanciaEntrePontos_AB(origemPonto), 
				outroPonto.distanciaEntrePontos_AB(origemPonto), 
				umPonto.distanciaEntrePontos_AB(outroPonto), 
				umPonto.getAbscissa(), umPonto.getOrdenada(),
				outroPonto.getAbscissa(), outroPonto.getOrdenada(),
				origemPonto.getAbscissa(), origemPonto.getOrdenada() );
		
	} // end main method
	
} // end class Principal
