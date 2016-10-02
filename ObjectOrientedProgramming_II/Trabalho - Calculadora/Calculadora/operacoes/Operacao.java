/**
 * Trabalho final da Disciplina de POO II
 */
package operacoes;

import pilha.Valor;

/**
 * Interface para compor as operações matematicas
 *
 * @author Professional
 */
public interface Operacao
{
	/**
	 * Realiza uma operação matemática de acordo como o delegação utilizada
	 *
	 * @return um objeto Valor como resultado da operação
	 */
	public Valor operacao();

	/**
	 * @return Diz se a operação é valida com os operandos fornecidos
	 */
	public boolean operandosValidos( Valor[] valor );

}
