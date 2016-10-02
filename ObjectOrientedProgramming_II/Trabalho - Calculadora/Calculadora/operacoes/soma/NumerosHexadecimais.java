/**
 * Trabalho final da Disciplina de POO II
 */

package operacoes.soma;

import java.math.BigDecimal;

import operacoes.Operacao;
import pilha.NumeroReal;
import pilha.Valor;

/**
 *
 *
 * @author Professional
 */
public class NumerosHexadecimais implements Operacao
{
	private Valor[] valor;

	/**
	 * Constrói um objeto dessa classe, null se ele for inválido
	 *
	 * @param valor
	 */
	public NumerosHexadecimais( Valor[] valor )
	{
		if( this.operandosValidos( valor ) == true )
		{
			this.valor = valor;

		}

		this.valor = new Valor[]
				{
					new NumeroReal( new BigDecimal( 0 ) ),
					new NumeroReal( new BigDecimal( 0 ) )

				};

	}

	/**
	 * @see operacoes.Operacao#operacao(pilha.Valor[])
	 */
	@Override
	public Valor operacao()
	{
		NumeroReal numero1 = ( NumeroReal ) valor[0].retorneElemento();

		NumeroReal numero2 = ( NumeroReal ) valor[1].retorneElemento();

		return new

		NumeroReal( numero1.retorneValor().add(numero2.retorneValor()) );

	}

	/**
	 * @see operacoes.Operacao#operandosValidos()
	 */
	@Override
	public boolean operandosValidos( Valor[] valor )
	{
	    if( valor.length != 2)
	    {
		    return false;

	    }

		NumeroReal numero1 = ( NumeroReal ) this.valor[0].retorneElemento();
		NumeroReal numero2 = ( NumeroReal ) this.valor[1].retorneElemento();

		boolean igual = true;

		igual = numero1.getClass().equals( numero2.getClass() );

		if( igual == false )
		{
			return false;

		}

		try
		{
			numero1.retorneValor();

		} catch ( Exception e )
		{
			return false;

		}

	    return igual;

	}

}
