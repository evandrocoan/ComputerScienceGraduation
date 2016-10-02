/**
 * Trabalho final da Disciplina de POO II
 */
package pilha;

/**
 * Interface para que qualquer tipo de dado possa ser utilizado na
 * composição da pilha
 *
 * @author Professional
 */
public interface Valor
{
	/**
	 * Retorna um objeto da classe de acordo com a delegação utilizada
	 *
	 * @return um objeto ValorArmazenado
	 */
	public Valor retorneElemento();

	/**
	 * Recebe um objeto da classe da delegação atualmente utilizada e o
	 * substitui.
	 * Acessa o objeto de acordo com o tipo e altera o valor do numero
	 * armazenado na pilha
	 *
	 * @return um booleano dizendo se a operação foi bem sucedida
	 * @param um objeto ValorArmazenado contendo o novo valor
	 */
	public boolean altereValor( Valor valor );

	/**
	 * Retorna uma String reprentando o objeto de acordo com a delegação
	 * utilizada
	 *
	 * @return uma String que representa o objeto
	 */
	@Override
	public String toString();

}
