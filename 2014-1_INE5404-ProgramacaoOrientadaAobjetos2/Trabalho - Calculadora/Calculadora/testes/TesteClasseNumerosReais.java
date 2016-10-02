package testes;

import java.math.BigDecimal;
import java.util.ArrayList;

import operacoes.Operacao;
import operacoes.soma.NumerosBinarios;
import operacoes.soma.NumerosComplexos;
import operacoes.soma.NumerosHexadecimais;
import operacoes.soma.NumerosReais;
import operacoes.soma.Soma;
import pilha.NumeroReal;
import pilha.Valor;

/**
 * @author Professional
 *
 */
public class TesteClasseNumerosReais
{
	private ArrayList< Valor > pilha = new ArrayList<Valor>();
	private ArrayList< Valor[] > desfazer = new ArrayList<Valor[]>();

	/**
	 * @param args
	 */
	public static void main( String[] args )
	{
		TesteClasseNumerosReais nada = new TesteClasseNumerosReais();
		nada.teste();

	}



	private void teste()
	{
		Valor valor1 = new NumeroReal( new BigDecimal( 1 ) );
		Valor valor2 = new NumeroReal( new BigDecimal( 9 ) );

		pilha.add( valor1 );
		pilha.add( valor2 );

		///////////////////Metodo Soma/////////////////////////

	}

}
