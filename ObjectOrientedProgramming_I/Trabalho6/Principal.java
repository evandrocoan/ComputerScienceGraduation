/*
 * Escreva uma classe que possibilite tratar um número complexo como objeto.
 * z = a + bi
 * a --> parte real
 * b --> parte imaginária
 * i --> (-1)^(1/2)
 * Ex:  2 - 3i
 * 4.8 + 3.9i
 * 
 * Coloque nesta classe (além de construtor) métodos para:
 * a) Retornar a parte real do complexo representado pelo objeto.
 * b) Retornar a parte imaginária do complexo representado pelo objeto.
 * c) Retornar conjugado do complexo representado pelo objeto. 
 *  (objeto da classe complexo)
 * d) Método que retorne a soma do complexo representado pelo objeto executor.
 * e) Idem para subtração, multiplicação e divisão.
 * f) Método que retorna o complexa na forma de String.
 * g) Método que retorne o módulo do número complexo representado pelo 
 *  objeto. ( (pr)^2 + (pi)^2 )^(1/2)
 * Observação: Escreva um método main para testar a sua classe. 
 *  (Solicita um complexo, bla, bla, ...)
 */

package Trabalho6;

/**
 *
 * @author Professional
 */
public class Principal 
{
	/**
    * Método principal que inicia a execução da aplicação Java
    * @param args
    */
   public static void main( String[] args )
	{
		Interface aInterface = new Interface();
      
      aInterface.monstrarMensagem( "Bem vindo ao teste da classe Complexo!" );
      
      Complexo complexo = aInterface.solicitarComplexo();
      Complexo complexo2 = aInterface.solicitarComplexo();
      
      aInterface.monstrarMensagem( "A parte real do primeiro número "
              + "complexo  solicitado é: " + complexo.getReal() 
              + "\nE a parte complexa é: " + complexo.getImaginario() 
              + "\nO conjugado dele é: " + complexo.getConjugado().paraString() 
              + "\nO seu módulo é: " + complexo.modulo()
              + "\nA soma do primeiro com o segundo número complexo é: " 
              + complexo.soma( complexo2 ).paraString() 
              + "\nE a diferença do primeiro "
              + "com o segundo número complexo é: " 
              + complexo.diferenca( complexo2 ).paraString() 
              + "\nE a multiplicação do "
              + "primeiro com o segundo número complexo é: " 
              + complexo.multiplicacao( complexo2 ).paraString() 
              + "\nE a divisão do "
              + "primeiro com o segundo número complexo é: " 
              + complexo.divisao( complexo2 ).paraString() );
		
	} // end main method
	
} // end class Principal
