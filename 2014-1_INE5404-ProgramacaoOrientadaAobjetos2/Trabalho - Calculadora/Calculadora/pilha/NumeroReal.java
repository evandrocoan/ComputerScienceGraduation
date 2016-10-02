/**
 * Trabalho final da Disciplina de POO II
 */

package pilha;

import java.math.BigDecimal;

/**
 * Classe que representa um número real usando a classe BigDecimal
 *
 * @author Professional
 */
public class NumeroReal implements Valor
{
	private BigDecimal valor;

	/**
	 * Contrutor padrão para o valor 0.0
	 */
	public NumeroReal()
	{
		this.valor = new BigDecimal( 0.0 );

	}

	/**
	 * Construtor para algum valor fornecido
	 *
	 * @param valor
	 */
	public NumeroReal( BigDecimal valor)
	{
		this.valor = valor;

	}

	/**
	 * Retorna o objeto dessa classe
	 *
	 * @return NumeroReal
	 * @see pilha.Valor#retorneElemento()
	 */
	@Override
	public Valor retorneElemento()
	{
		return new NumeroReal( this.valor );

	}

	/**
	 * @see pilha.Valor#altereValor(pilha.Valor)
	 */
	@Override
	public boolean altereValor( Valor valor )
	{

		BigDecimal temp = ( ( NumeroReal ) valor ).retorneValor();

		if( !temp.getClass().equals("class BigDecimal") )
		{
			return false;

		}

		this.valor = temp;

		return true;

	}

	/**
	 * Retorna o valor armazenado nesse objeto
	 *
	 * @return the valor
	 */
	public BigDecimal retorneValor()
	{
		return this.valor;

	}

	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString()
	{
		return this.valor.toString();

	}

}
