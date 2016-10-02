/*
 * Escreva um método main que via interface, solicite os 
 *  dados de uma conta e instancie um objeto da classe ContaCorrente. 
 * Deve ser solicidado uma única conta. Após obter 
 *  a conta-corrente, deve ser apresentado ao usuário o seguinte 
 *  Menu de opções:
 * 1 - Efetura depósito
 * 2 - Efetuar saque
 * 3 - Consultar saldo
 * 4 - Efetuar transferência
 * 5 - Finalizar
 * Obs: Caso o usuário escolha opção 4, deve ser solicidado ao 
 *  usuário qual a outra conta.
 */

package Trabalho5;

/**
 * Responsável por iniciar o programa
 * @author Professional
 */
public class Principal 
{
   /**
   * Método main que via interface, solicita os dados de uma conta e 
   * instancia um objeto da classe ContaCorrente. 
   * Será solicidado uma única conta. Após obter 
   *  a conta-corrente, deve ser apresentado ao usuário o seguinte 
   *  Menu de opções:
   * 1 - Efetuar depósito
   * 2 - Efetuar saque
   * 3 - Consultar saldo
   * 4 - Efetuar transferência
   * 5 - Finalizar
   * Obs: Caso o usuário escolha opção 4, deve ser solicidado ao 
   *  usuário qual a outra conta.
   * @param args
   */
   public static void main( String[] args )
	{
		Interface aInterface = new Interface();
      
      aInterface.monstrarMensagem( "Bem vindo ao Sistema de cadastro de "
              + "conta corrente!\n Clique OK para continuar." );
      
      ContaCorrente conta = aInterface.cadastrarContaCorrente();
      
      int opcao = 0;
		
      // bloco que permite o usuário controlar o programa
      while( opcao != 5 )
      {
         opcao = aInterface.obterOpcao();
         
         // bloco 1 - Efetuar depósito
         if( opcao == 1 )
         {
            aInterface.monstrarMensagem( 
                    aInterface.depositarDinheiro( conta ) );
            
         } // termina bloco 1 - Efetuar depósito
         
         // bloco 2 - Efetuar saque
         if( opcao == 2 )
         {
            aInterface.monstrarMensagem( 
                    aInterface.sacarDinheiro( conta ) );
            
         } // termina bloco 2 - Efetuar saque
         
         // bloco 3 - Consultar saldo
         if( opcao == 3 )
         {
            aInterface.monstrarMensagem( "O saldo da conta corrente é: " 
                   + conta.getSaldo() );
            
         } // termina bloco 3 - Consultar saldo
         
         // bloco 4 - Efetuar transferência
         if( opcao == 4 )
         {
            ContaCorrente contaDestino = aInterface.cadastrarContaCorrente();
            
            aInterface.monstrarMensagem( 
                    aInterface.transferenciaDinheiro( conta, contaDestino ) );
            
         } // termina bloco 4 - Efetuar transferência
         
      } // termina bloco que dá controle do usuário ao programa
      
	} // end main method
	
} // end class Principal
