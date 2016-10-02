/**
 * Trabalho final da Disciplina de POO II
 */

package operacoes.soma;

import operacoes.Operacao;
import pilha.Valor;

/**
 * Realiza a soma de dois números
 *
 * @author Professional
 */
public class Soma implements Operacao
{
	Operacao soma = null;

	/**
	 * Um contrutor padrão para o processo de delegação
	 *
	 * @param o valor para contrução do objeto
	 */
	public Soma( Operacao soma )
	{
		this.soma = soma;

	}

	/**
	 * @see operacoes.Operacao#operacao(pilha.Valor[])
	 */
	@Override
	public Valor operacao()
	{
		return this.soma.operacao();

	}

	/**
	 * @see operacoes.Operacao#operandosValidos()
	 */
	@Override
	public boolean operandosValidos( Valor[] valor )
	{
		return this.soma.operandosValidos( valor );
	}

}
